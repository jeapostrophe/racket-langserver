#lang racket/base

;; Check Syntax facts form a star: one def, many uses. This service stores
;; those stars as a graph so questions about defs, uses, and their ranges
;; stay efficient. It answers live-position questions on that graph and
;; returns CharRange values, plus a Module-Binding when a cross-document
;; identity exists. Callers never hold ids.
;;
;; An id is a private integer for one occurrence in the accepted snapshot:
;; a def-id for a local def, a use-id for a use. When the def lives in this
;; file, the star's def is that def-id. When it does not, the def is the
;; Module-Binding itself. Module-Binding is also the only identity that
;; leaves this document.
;;
;; Positions split into live keys and snapshot values. expand and contract
;; move the interval-map keys, while id->range stays at the accepted
;; snapshot until a query replays the position journal into a live
;; CharRange. The query pos is already live, so it hits those keys
;; directly. Edits do not rewrite id->range or the graph. Ranges here are
;; CharRange (absolute character offsets), not LSP Range. Typing in the
;; middle of a name uses #:interior 'extend so the occurrence stays one
;; range.
;;
;; Check Syntax fills the star in any order. A non-require arrow links a
;; use-id to a def-id. Jump links a use-id to a Module-Binding, or to a
;; this-file def-id if definition target already ran. Definition target
;; attaches Module-Binding to that def-id and moves earlier jump uses onto
;; it, so both callback orders produce the same star. Require arrows are
;; ignored.

(require "interface.rkt"
         "interval-map-edit.rkt"
         "position-journal.rkt"
         "../../common/interfaces.rkt"
         "../internal-types.rkt"
         data/interval-map
         drracket/check-syntax
         racket/class
         racket/dict
         racket/list)

(provide declaration%)

;; Insertion grows a stored range only when it is strictly inside. At the start
;; boundary the whole range moves, matching interval-map right gravity.
(define (expand-char-range range start end)
  (define increase (- end start))
  (define range-start (CharRange-start range))
  (define range-end (CharRange-end range))
  (CharRange (if (>= range-start start)
                 (+ range-start increase)
                 range-start)
             (if (> range-end start)
                 (+ range-end increase)
                 range-end)))

;; Positions inside deleted text collapse to the deletion start. A range
;; disappears when both bounds collapse to the same position.
(define (contract-char-range range start end)
  (define decrease (- end start))
  (define (contract-position position)
    (cond
      [(<= position start) position]
      [(>= position end) (- position decrease)]
      [else start]))
  (define range-start (contract-position (CharRange-start range)))
  (define range-end (contract-position (CharRange-end range)))
  (and (< range-start range-end)
       (CharRange range-start range-end)))

(define declaration%
  (class base-service%
    (init-field src)
    (super-new)

    ;; Ids and graph tables are private. Module-Binding is the only identity
    ;; returned to callers, and only position queries can reach this graph.
    (define next-id 0)
    (define def-at-position (make-interval-map))
    (define use-at-position (make-interval-map))
    (define id->range (make-hash))
    (define def->use-ids (make-hash))
    (define use->def (make-hash))
    ;; this-file Module-Binding -> def-id; reverse is def-id -> Module-Binding
    (define module-binding->def-id (make-hash))
    (define def-id->module-binding (make-hash))
    (define range-journal (make-position-journal))

    (define/private (fresh-id!)
      (define id next-id)
      (set! next-id (add1 next-id))
      id)

    (define/private (normalize-end start end)
      (if (= start end)
          (add1 end)
          end))

    (define/private (def-at-exact-range start end)
      (define-values (found-start found-end def-id)
        (interval-map-ref/bounds def-at-position start #f))
      (and (eqv? found-start start)
           (eqv? found-end end)
           def-id))

    (define/private (get-or-create-def start end)
      (or (def-at-exact-range start end)
          (let ([def-id (fresh-id!)])
            (interval-map-set! def-at-position start end def-id)
            (hash-set! id->range def-id (CharRange start end))
            (hash-set! def->use-ids def-id '())
            def-id)))

    (define/private (alloc-use start end)
      (define use-id (fresh-id!))
      (interval-map-set! use-at-position start end use-id)
      (hash-set! id->range use-id (CharRange start end))
      use-id)

    ;; `def` is a def-id or a Module-Binding (imported uses).
    (define/private (link-use! use-id def)
      (hash-set! use->def use-id def)
      (hash-update! def->use-ids def (lambda (use-ids) (cons use-id use-ids)) '()))

    (define/private (live-range-for-id id)
      (position-journal-replay range-journal
                               (hash-ref id->range id)
                               expand-char-range
                               contract-char-range))

    (define/private (sort-ranges ranges)
      (sort ranges
            (lambda (left right)
              (or (< (CharRange-start left) (CharRange-start right))
                  (and (= (CharRange-start left) (CharRange-start right))
                       (< (CharRange-end left) (CharRange-end right)))))))

    ;; Def at pos: def-id, Module-Binding, or #f.
    (define/private (def-at pos)
      (define use-id (interval-map-ref use-at-position pos #f))
      (if use-id
          (hash-ref use->def use-id #f)
          (interval-map-ref def-at-position pos #f)))

    (define/private (def->module-binding def)
      (if (Module-Binding? def)
          def
          (hash-ref def-id->module-binding def #f)))

    ;; Live absolute position in; live CharRange out.
    (define/public (occurrence-at pos)
      (define-values (use-start use-end use-id)
        (interval-map-ref/bounds use-at-position pos #f))
      (define-values (def-start def-end def-id)
        (interval-map-ref/bounds def-at-position pos #f))
      (cond
        [use-id (CharRange use-start use-end)]
        [def-id (CharRange def-start def-end)]
        [else #f]))

    ;; Returns the live same-document definition range at pos, or #f for an
    ;; imported binding or a position with no occurrence.
    (define/public (definition-at pos)
      (define def (def-at pos))
      (and def
           (not (Module-Binding? def))
           (live-range-for-id def)))

    ;; Returns live use ranges for the definition resolved at pos. The
    ;; def itself is not included.
    (define/public (uses-at pos)
      (define def (def-at pos))
      (if def
          (sort-ranges
            (remove-duplicates
              (for*/list ([use-id (in-list (hash-ref def->use-ids def '()))]
                          [range (in-value (live-range-for-id use-id))]
                          #:when range)
                range)))
          '()))

    ;; Returns the cross-document identity at pos when the resolved definition
    ;; has one. Pure lexical bindings return #f.
    (define/public (module-binding-at pos)
      (define def (def-at pos))
      (and def (def->module-binding def)))

    ;; Returns live use ranges paired with their cross-document identities.
    ;; Pure lexical uses are omitted from contribution publication.
    (define/public (module-binding-uses)
      (for*/list ([(range use-id) (in-dict use-at-position)]
                  [def (in-value (hash-ref use->def use-id #f))]
                  [module-binding (in-value (and def (def->module-binding def)))]
                  #:when module-binding)
        (cons (CharRange (car range) (cdr range)) module-binding)))

    ;; Returns live definition ranges paired with their cross-document identities.
    ;; Pure local definitions are omitted from contribution publication.
    (define/public (module-binding-definitions)
      (for*/list ([(module-binding def-id) (in-hash module-binding->def-id)]
                  [range (in-value (live-range-for-id def-id))]
                  #:when range)
        (cons range module-binding)))

    ;; Attach Module-Binding to a this-file def-id and unify same-file jumps.
    (define/private (attach-module-binding! def-id module-binding)
      (define old-module-binding
        (hash-ref def-id->module-binding def-id #f))
      (when old-module-binding
        (hash-remove! module-binding->def-id old-module-binding))
      (hash-set! def-id->module-binding def-id module-binding)
      (hash-set! module-binding->def-id module-binding def-id)

      ;; Jump may run before definition target. Move those uses onto the local
      ;; def-id and drop the Module-Binding key in def->use-ids.
      (define imported-use-ids
        (hash-ref def->use-ids module-binding '()))
      (unless (null? imported-use-ids)
        (for ([use-id (in-list imported-use-ids)])
          (hash-set! use->def use-id def-id))
        (hash-update! def->use-ids
                      def-id
                      (lambda (use-ids) (append imported-use-ids use-ids))
                      '())
        (hash-remove! def->use-ids module-binding)))

    (define/override (reset)
      (set! next-id 0)
      (set! def-at-position (make-interval-map))
      (set! use-at-position (make-interval-map))
      (set! id->range (make-hash))
      (set! def->use-ids (make-hash))
      (set! use->def (make-hash))
      (set! module-binding->def-id (make-hash))
      (set! def-id->module-binding (make-hash))
      (position-journal-reset! range-journal))

    (define/override (expand start end)
      (interval-map-expand/policy! def-at-position start end #:interior 'extend)
      (interval-map-expand/policy! use-at-position start end #:interior 'extend)
      (position-journal-record-expand! range-journal start end))

    (define/override (contract start end)
      (interval-map-contract! def-at-position start end)
      (interval-map-contract! use-at-position start end)
      (position-journal-record-contract! range-journal start end))

    (define/override (syncheck:add-jump-to-definition/phase-level+space
                       _src-obj start end id filename submods phase+space)
      (define normalized-end (normalize-end start end))
      (define use-id (alloc-use start normalized-end))
      (define module-binding (Module-Binding filename submods phase+space id))
      ;; Local def-id if definition target has run; else Module-Binding as the def.
      (define def (hash-ref module-binding->def-id module-binding module-binding))
      (link-use! use-id def))

    (define/override (syncheck:add-definition-target/phase-level+space
                       _src-obj start end id submods phase+space)
      (define normalized-end (normalize-end start end))
      (define def-id (get-or-create-def start normalized-end))
      (define module-binding (Module-Binding src submods phase+space id))
      (attach-module-binding! def-id module-binding))

    (define/override (syncheck:add-arrow/name-dup _start-src-obj start-left start-right
                                                  _end-src-obj end-left end-right
                                                  _actual? _phase-level
                                                  require-arrow? _name-dup?)
      (unless require-arrow?
        (define def-id (get-or-create-def start-left (normalize-end start-left start-right)))
        (define use-id (alloc-use end-left (normalize-end end-left end-right)))
        (link-use! use-id def-id)))))
