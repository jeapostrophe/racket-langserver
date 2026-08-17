#lang racket/base

;; Binding graph for one document. Answers: which occurrence is at a
;; position, the local definition range, other uses in this document, and a
;; Module-Binding when the name can leave this document.
;;
;; Check Syntax reports one definition and many uses. This service stores
;; the Check Syntax report as a graph. Each occurrence has a private integer
;; id. Callers never hold those ids. Uses share a graph owner: a local
;; def-id when the definition is in this document, a Module-Binding when
;; the name is imported, or a Module-Lang-Use-Key for a leftover `#lang`
;; export use. Only Module-Binding may leave this document.
;;
;; Check Syntax positions are a snapshot. After edits, def-at-position and
;; use-at-position move with the text. id->range stays at the snapshot
;; until a query replays a position journal into a live CharRange
;; (character offsets, not an LSP line/character Range). The query
;; position is already live, so it hits the moved maps directly.
;;
;; Callbacks may arrive in any order. Lexical arrows, jumps, and definition
;; targets form one graph either way. Module-language arrows mark `#lang`
;; export uses. Those arrows wait until walk-stx, then fill only ranges
;; that lexical arrows and jumps left free. A leftover `#lang` use has no
;; local definition and no Module-Binding, so it must not leave this
;; document.

(require "interface.rkt"
         "interval-map-edit.rkt"
         "position-journal.rkt"
         "../../common/interfaces.rkt"
         "../internal-types.rkt"
         data/interval-map
         drracket/check-syntax
         racket/class
         racket/dict
         racket/list
         racket/match)

(provide declaration%)

;; Private graph owner for a leftover `#lang` export use. Not a
;; Module-Binding. Do not return this key from public methods or treat the
;; key as a definition. language-start/end is the `#lang` range Check Syntax
;; reported as the arrow start. use-text is the use-site text, not the
;; language name. equal? groups leftover `#lang` uses that share the same
;; `#lang` range, phase-level, and use-site text.
(struct Module-Lang-Use-Key
  (language-start language-end phase-level use-text)
  #:transparent)

;; A `'module-lang` use held until walk-stx, so lexical arrows and jumps can
;; claim the range first.
(struct Pending-Module-Lang-Use
  (key use-start use-end)
  #:transparent)

;; Invariant: journal replay must move a snapshot CharRange the same way
;; expand and contract move the interval maps. An insert extends a stored
;; CharRange only when the insert starts strictly inside that CharRange,
;; matching #:interior 'extend.
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

;; Positions inside deleted text collapse to the deletion start. A CharRange
;; that collapses to empty returns #f; later journal steps cannot restore it.
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
    (init-field src doc-text)
    (super-new)

    (define def-at-position (make-interval-map))
    (define use-at-position (make-interval-map))
    ;; Snapshot CharRange for each id. Edits do not rewrite id->range;
    ;; queries replay range-journal instead.
    (define id->range (make-hash))
    (define def->use-ids (make-hash))
    (define use->def (make-hash))
    ;; This document's definitions that have a Module-Binding. Imports are not
    ;; in these two hashes; an import uses the Module-Binding itself as the
    ;; graph owner.
    (define module-binding->def-id (make-hash))
    (define def-id->module-binding (make-hash))
    (define range-journal (make-position-journal))
    ;; `'module-lang` uses held until walk-stx.
    (define pending-module-lang-uses '())

    (define fresh-id!
      (let ([id 0])
        (lambda ()
          (begin0 id
            (set! id (add1 id))))))

    ;; Lexical arrows and jumps treat a zero-width Check Syntax report as a
    ;; one-character range. Module-language arrows do not call this function.
    ;; A zero-width module-language range is dropped instead.
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

    ;; Reuse the def-id for this snapshot range so later arrows and
    ;; definition targets join the same definition.
    (define/private (get-or-create-def start end)
      (or (def-at-exact-range start end)
          (let ([def-id (fresh-id!)])
            (interval-map-set! def-at-position start end def-id)
            (hash-set! id->range def-id (CharRange start end))
            (hash-set! def->use-ids def-id '())
            def-id)))

    (define/private (use-at-exact-range start end)
      (define-values (found-start found-end use-id)
        (interval-map-ref/bounds use-at-position start #f))
      (and (eqv? found-start start)
           (eqv? found-end end)
           use-id))

    (define/private (alloc-use start end)
      (define use-id (fresh-id!))
      (interval-map-set! use-at-position start end use-id)
      (hash-set! id->range use-id (CharRange start end))
      use-id)

    ;; `def` is the graph owner: a local def-id, a Module-Binding, or a
    ;; Module-Lang-Use-Key.
    (define/private (link-use! use-id def)
      (hash-set! use->def use-id def)
      (hash-update! def->use-ids def (lambda (use-ids) (cons use-id use-ids)) '()))

    ;; Replay the snapshot CharRange in id->range through range-journal.
    ;; Returns #f if an edit removed the range.
    (define/private (live-range-for-id id)
      (position-journal-replay range-journal
                               (hash-ref id->range id)
                               expand-char-range
                               contract-char-range))

    ;; Install a held `'module-lang` use only when no use already occupies
    ;; the pending use's exact snapshot range. Do not overwrite a lexical
    ;; arrow or a jump.
    (define/private (apply-pending-module-lang-uses!)
      (for ([pending (in-list pending-module-lang-uses)])
        (define use-start (Pending-Module-Lang-Use-use-start pending))
        (define use-end (Pending-Module-Lang-Use-use-end pending))
        (unless (use-at-exact-range use-start use-end)
          (define use-id (alloc-use use-start use-end))
          (link-use! use-id (Pending-Module-Lang-Use-key pending))))
      (set! pending-module-lang-uses '()))

    (define/private (sort-ranges ranges)
      (sort ranges
            (lambda (left right)
              (or (< (CharRange-start left) (CharRange-start right))
                  (and (= (CharRange-start left) (CharRange-start right))
                       (< (CharRange-end left) (CharRange-end right)))))))

    (define/private (def-at pos)
      (define use-id (interval-map-ref use-at-position pos #f))
      (if use-id
          (hash-ref use->def use-id #f)
          (interval-map-ref def-at-position pos #f)))

    (define/private (def->module-binding def)
      (cond
        [(Module-Binding? def) def]
        [(exact-nonnegative-integer? def)
         (hash-ref def-id->module-binding def #f)]
        [else #f]))

    (define/public (occurrence-at pos)
      (define-values (use-start use-end use-id)
        (interval-map-ref/bounds use-at-position pos #f))
      (define-values (def-start def-end def-id)
        (interval-map-ref/bounds def-at-position pos #f))
      (cond
        [use-id (CharRange use-start use-end)]
        [def-id (CharRange def-start def-end)]
        [else #f]))

    ;; Same-document definition range, or #f. Only a local def-id has one.
    ;; An import or a Module-Lang-Use-Key has no definition in this document.
    (define/public (definition-at pos)
      (define def (def-at pos))
      (and (exact-nonnegative-integer? def)
           (live-range-for-id def)))

    ;; Live use ranges for the graph owner at pos, including leftover
    ;; `#lang` uses. Do not skip imported owners or leftover `#lang` owners.
    ;; Do not include the owner's definition range.
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

    ;; Cross-document identity at pos, or #f. Local-only names and
    ;; Module-Lang-Use-Key owners return #f.
    (define/public (module-binding-at pos)
      (define def (def-at pos))
      (and def (def->module-binding def)))

    ;; Each result is (cons live-use-range Module-Binding). Local-only uses and
    ;; leftover `#lang` uses are omitted; those names must not leave this
    ;; document.
    (define/public (module-binding-uses)
      (for*/list ([(range use-id) (in-dict use-at-position)]
                  [def (in-value (hash-ref use->def use-id #f))]
                  [module-binding (in-value (and def (def->module-binding def)))]
                  #:when module-binding)
        (cons (CharRange (car range) (cdr range)) module-binding)))

    ;; Each result is (cons live-definition-range Module-Binding). Local-only
    ;; definitions are omitted; those names must not leave this document.
    (define/public (module-binding-definitions)
      (for*/list ([(module-binding def-id) (in-hash module-binding->def-id)]
                  [range (in-value (live-range-for-id def-id))]
                  #:when range)
        (cons range module-binding)))

    ;; Record the Module-Binding for a definition in this document, then merge
    ;; any jumps that arrived before the definition target.
    (define/private (attach-module-binding! def-id module-binding)
      (define old-module-binding
        (hash-ref def-id->module-binding def-id #f))
      (when old-module-binding
        (hash-remove! module-binding->def-id old-module-binding))
      (hash-set! def-id->module-binding def-id module-binding)
      (hash-set! module-binding->def-id module-binding def-id)

      ;; A jump may run before the definition target. Move uses still keyed by
      ;; the Module-Binding onto the local def-id, then drop that Module-Binding
      ;; key from def->use-ids. Otherwise uses-at would miss those uses after
      ;; the graph owner changes.
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
      (set! def-at-position (make-interval-map))
      (set! use-at-position (make-interval-map))
      (set! id->range (make-hash))
      (set! def->use-ids (make-hash))
      (set! use->def (make-hash))
      (set! module-binding->def-id (make-hash))
      (set! def-id->module-binding (make-hash))
      (set! pending-module-lang-uses '())
      (position-journal-reset! range-journal))

    ;; Apply leftover `#lang` uses after lexical arrows and jumps have claimed
    ;; ranges. ExpandResult is unused. Must run before any query or edit:
    ;; pending use ranges are still Check Syntax snapshot coordinates.
    (define/override (walk-stx _expand-result)
      (apply-pending-module-lang-uses!))

    (define/override (expand start end)
      ;; Typing inside a name must stay one occurrence. Do not use the
      ;; interval-map default, which splits the range.
      (interval-map-expand/policy! def-at-position start end #:interior 'extend)
      (interval-map-expand/policy! use-at-position start end #:interior 'extend)
      (position-journal-record-expand! range-journal start end))

    (define/override (contract start end)
      ;; Do not rewrite id->range here. Position maps shrink or drop ranges now.
      ;; Queries replay range-journal to move snapshot CharRanges.
      (interval-map-contract! def-at-position start end)
      (interval-map-contract! use-at-position start end)
      (position-journal-record-contract! range-journal start end))

    (define/override (syncheck:add-jump-to-definition/phase-level+space
                       _src-obj start end id filename submods phase+space)
      (define normalized-end (normalize-end start end))
      (define use-id (alloc-use start normalized-end))
      (define module-binding (Module-Binding filename submods phase+space id))
      ;; Local def-id if the definition target has already run; otherwise
      ;; the Module-Binding itself is the graph owner until attach-module-binding! runs.
      (define def (hash-ref module-binding->def-id module-binding module-binding))
      (link-use! use-id def))

    (define/override (syncheck:add-definition-target/phase-level+space
                       _src-obj start end id submods phase+space)
      (define normalized-end (normalize-end start end))
      (define def-id (get-or-create-def start normalized-end))
      (define module-binding (Module-Binding src submods phase+space id))
      (attach-module-binding! def-id module-binding))

    (define/override (syncheck:unused-binder _src-obj start end)
      ;; An unused lexical binder has no arrow, so this callback is its only
      ;; entry into the graph. Module definitions reuse the same range.
      (get-or-create-def start (normalize-end start end))
      (void))

    (define/override (syncheck:add-arrow/name-dup _start-src-obj start-left start-right
                                                  _end-src-obj end-left end-right
                                                  _actual? phase-level
                                                  require-arrow? _name-dup?)
      (match require-arrow?
        [#f
         (define def-id (get-or-create-def start-left (normalize-end start-left start-right)))
         (define use-id (alloc-use end-left (normalize-end end-left end-right)))
         (link-use! use-id def-id)]
        ['module-lang
         ;; Chosen over `normalize-end`: a zero-width report means disappeared
         ;; syntax, not a one-character name. Keep punctuation ranges that
         ;; Check Syntax reported, such as quote.
         (when (< end-left end-right)
           (define key
             (Module-Lang-Use-Key start-left
                                  start-right
                                  phase-level
                                  (send doc-text get-text end-left end-right)))
           (set! pending-module-lang-uses
                 (cons (Pending-Module-Lang-Use key end-left end-right)
                       pending-module-lang-uses)))]
        [_
         ;; Ordinary require arrows are ignored. Imported uses come from
         ;; syncheck:add-jump-to-definition.
         (void)]))))
