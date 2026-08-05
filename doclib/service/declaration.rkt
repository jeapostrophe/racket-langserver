#lang racket/base

(require "interface.rkt"
         racket/class
         "../internal-types.rkt"
         data/interval-map
         racket/dict
         racket/set
         drracket/check-syntax)

(provide declaration%)

(define declaration%
  (class base-service%
    (super-new)
    ;; decl -> (set pos ...)
    (define sym-decls (make-interval-map))
    ;; pos -> decl
    (define sym-bindings (make-interval-map))

    (define/override (get)
      (list sym-decls sym-bindings))

    ;; Named read for document assembly. Returns start, end, and Decl for the
    ;; binding at `pos`. At declaration positions, find the Decl through a first
    ;; use. Returns #f #f #f when nothing matches.
    (define/public (declaration-at pos)
      (define-values (start end maybe-decl)
        (interval-map-ref/bounds sym-bindings pos #f))
      (define-values (bind-start bind-end maybe-bindings)
        (interval-map-ref/bounds sym-decls pos #f))
      (cond
        [maybe-decl (values start end maybe-decl)]
        [maybe-bindings
         (define decl
           (interval-map-ref sym-bindings
                             (car (set-first maybe-bindings))
                             #f))
         (if decl
             (values bind-start bind-end decl)
             (values #f #f #f))]
        [else (values #f #f #f)]))

    (define/override (reset)
      (set! sym-decls (make-interval-map))
      (set! sym-bindings (make-interval-map)))

    (define/override (expand start end)
      (define inc (- end start))
      (move-interior-intervals sym-decls (- start 1) inc)
      (move-interior-intervals sym-bindings (- start 1) inc)
      (map (lambda (int-map) (interval-map-expand! int-map start end))
           (list sym-decls sym-bindings)))

    (define/override (contract start end)
      (define dec (- start end))
      (move-interior-intervals sym-decls end dec)
      (move-interior-intervals sym-bindings end dec)
      (map (lambda (int-map) (interval-map-contract! int-map start end))
           (list sym-decls sym-bindings)))

    ;; some intervals are held inside of the interval maps... so we need to expand/contract these manually
    (define/private (move-interior-intervals int-map after amt)
      (dict-for-each int-map
                     (lambda (range decl-set)
                       (define result (cond
                                        [(Decl? decl-set)
                                         (define d-range (cons (Decl-left decl-set) (Decl-right decl-set)))
                                         (if (and (not (Decl-filepath decl-set))
                                                  (> (car d-range) after))
                                             (struct-copy Decl decl-set
                                               [left (+ (car d-range) amt)]
                                               [right (+ (cdr d-range) amt)])
                                             #f)]
                                        [else
                                         (list->set (set-map decl-set (lambda (d-range)
                                                                        (if (> (car d-range) after)
                                                                            (cons (+ (car d-range) amt) (+ (cdr d-range) amt))
                                                                            d-range))))]))
                       (when result
                         (interval-map-set! int-map (car range) (cdr range) result)))))

    (define/override (syncheck:add-jump-to-definition/phase-level+space
                       _src-obj start end id filename submods phase+space)
      (define decl (Decl filename submods phase+space id 0 0))
      ;; NOTE start <= end. In some situations, it may be that start = end.
      (interval-map-set! sym-bindings start (if (= start end) (add1 end) end) decl))

    (define/override (syncheck:add-arrow/name-dup _start-src-obj start-left start-right
                                                  _end-src-obj end-left end-right
                                                  _actual? _phase-level
                                                  require-arrow? _name-dup?)
      (when (= start-left start-right)
        (set! start-right (add1 start-right)))
      (when (= end-left end-right)
        (set! end-right (add1 end-right)))
      ;; Mapping from doc declaration to set of bindings.
      (define prev-bindings (interval-map-ref sym-decls start-left set))
      (define new-bindings (set-add prev-bindings (cons end-left end-right)))
      (interval-map-set! sym-decls start-left start-right new-bindings)
      ;; Mapping from binding to declaration.
      (unless require-arrow?
        (define new-decl (Decl #f #f #f #f start-left start-right))
        (interval-map-set! sym-bindings end-left end-right new-decl)))
    ))

