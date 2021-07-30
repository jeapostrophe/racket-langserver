#lang racket/base

;; from https://github.com/yjqww6/drcomplete
;;
;; drcomplete
;; Copyright (c) 2019 yjqww6
;;
;; This package is distributed under the GNU Lesser General Public
;; License (LGPL).  This means that you can link drcomplete into proprietary
;; applications, provided you follow the rules stated in the LGPL.  You
;; can also modify this package; if you distribute a modified version,
;; you must distribute it under the terms of the LGPL, which in
;; particular means that you must release the source code for the
;; modified software.  See http://www.gnu.org/copyleft/lesser.html
;; for more information.

(require (for-syntax racket/base)
         racket/set
         syntax/kerncase
         racket/sequence
         racket/bool)
(provide walk walk-module)

(define (walk* stxs phase)
  (let loop ([ls (syntax->list stxs)])
    (if (null? ls)
        (seteq)
        (set-union (walk (car ls) phase) (loop (cdr ls))))))

(define (visible? id)
  (for/and ([scope (in-list
                    (hash-ref (syntax-debug-info id)
                              'context (λ () '())))])
    (not (eq? 'macro (vector-ref scope 1)))))

(define (visible stx)
  (syntax-case stx ()
    [(a . b)
     (set-union (visible #'a) (visible #'b))]
    [x
     (identifier? #'x)
     (if (visible? #'x)
         (seteq (syntax-e #'x))
         (seteq))]
    [_ (seteq)]))

(define-syntax (for/union stx)
  (syntax-case stx ()
    [(_ clauses body ... expr)
     (with-syntax ([orig stx])
       #'(for/fold/derived orig ([s (set)])
           clauses
           body ...
           (set-union s expr)))]))

(define (sym=? a b)
  (symbol=? (syntax-e a) (syntax-e b)))


(define (walk stx [phase (namespace-base-phase)])
  (define sym-set
    (set-union
     (kernel-syntax-case/phase
      stx phase
      [(#%expression ?expr) (walk #'?expr phase)]
      [(module _ _ (_ ?module-level-form ...))
       (walk* #'(?module-level-form ...) 0)]
      [(module* _ #f (_ ?module-level-form ...))
       (walk* #'(?module-level-form ...) phase)]
      [(module* _ _ (_ ?module-level-form ...))
       (walk* #'(?module-level-form ...) 0)]
      [(begin ?expr ...)
       (walk* #'(?expr ...) phase)]
      [(begin0 ?expr ...)
       (walk* #'(?expr ...) phase)]
      [(begin-for-syntax ?expr ...)
       (walk* #'(?expr ...) (+ phase 1))]
      [(define-values (?id ...) ?expr)
       (set-union (visible #'(?id ...)) (walk #'?expr phase))]
      [(define-syntaxes (?id ...) ?expr)
       (set-union (visible #'(?id ...)) (walk #'?expr (+ phase 1)))]
      [(#%plain-lambda ?formals ?expr ...)
       (set-union (visible #'?formals) (walk* #'(?expr ...) phase))]
      [(case-lambda (?formals ?expr ...) ...)
       (set-union (visible #'(?formals ...)) (walk* #'(?expr ... ...) phase))]
      [(if ?expr ...)
       (walk* #'(?expr ...) phase)]
      [(let-values ([(?id ...) ?expr] ...)
         ?body ...)
       (set-union (visible #'(?id ... ...))
                  (walk* #'(?expr ... ?body ...) phase))]
      [(letrec-values ([(?id ...) ?expr] ...)
         ?body ...)
       (set-union (visible #'(?id ... ...))
                  (walk* #'(?expr ... ?body ...) phase))]
      [(set! ?id ?expr)
       (walk #'?expr phase)]
      [(with-continuation-mark ?expr ...)
       (walk* #'(?expr ...) phase)]
      [(#%plain-app ?expr ...)
       (walk* #'(?expr ...) phase)]
      [_ (seteq)])
     (visible (syntax-property stx 'disappeared-binding))))
  sym-set)

(define (walk-module fpe)

  (define declared-modules (mutable-set))

  (define ids (mutable-set))

  (define alls (mutable-set))
  (define prefixs (mutable-set))
  (define all-excepts (mutable-set))
  (define prefix-all-excepts (mutable-set))

  (define (push! sth)
    (when (ext-module-path? sth)
      (set-add! declared-modules (syntax->datum sth))))

  (define (ext-module-path? r)
    (syntax-case* r (submod quote)
      sym=?
      [(submod "." _ ...) #f]
      [(submod ".." _ ...) #f]
      [(submod (quote _) _ ...) #f]
      [(quote x) (module-predefined? r)]
      [_ #t]))

  (define (phaseless-spec spec just)
    (define-syntax-rule (with-datum ([id0 exp0] [id exp] ...) body ...)
      (let ([id0 (syntax->datum exp0)])
        (when (ext-module-path? id0)
          (let ([id (syntax->datum exp)] ...)
            body ...))))

    (syntax-case* spec
      (only prefix all-except prefix-all-except rename)
      sym=?
      [(only ?raw-module-path ?id ...)
       (when (visible? #'?raw-module-path)
         (with-datum ([mod #'?raw-module-path]
                      [id* #'(?id ...)])
           (for ([id (in-list id*)])
             (set-add! ids id))))]
      [(prefix ?prefix-id ?raw-module-path)
       (when (visible? #'?raw-module-path)
         (with-datum ([mod #'?raw-module-path]
                      [pre #'?prefix-id])
           (set-add! declared-modules mod)
           (set-add! prefixs (list* just mod pre))))]
      [(all-except ?raw-module-path ?id ...)
       (when (visible? #'?raw-module-path)
         (with-datum ([mod #'?raw-module-path]
                      [id* #'(?id ...)])
           (set-add! declared-modules mod)
           (set-add! all-excepts (list* just mod id*))))]
      [(prefix-all-except ?prefix-id ?raw-module-path ?id ...)
       (when (visible? #'?raw-module-path)
         (with-datum ([mod #'?raw-module-path]
                      [pre #'?prefix-id]
                      [id* #'(?id ...)])
           (set-add! declared-modules mod)
           (set-add! prefix-all-excepts (list* just mod pre id*))))]
      [(rename ?raw-module-path ?id _)
       (with-datum ([mod #'?raw-module-path]
                    [id #'?id])
         (when (visible? #'?id)
           (set-add! ids id)))]
      [?raw-module-path
       (when (visible? #'?raw-module-path)
         (with-datum ([mod #'?raw-module-path])
           (set-add! declared-modules mod)
           (set-add! alls (cons just mod))))]))

  (define (each f syn . args)
    (for ([s (in-syntax syn)])
      (apply f s args)))

  (define (raw-require-spec spec)
    (define (maybe-just-meta spec)
      (syntax-case* spec (just-meta) sym=?
        [(just-meta ?n ?phaseless-spec* ...)
         (let ([n (syntax-e #'?n)])
           (when n
             (each phaseless-spec #'(?phaseless-spec* ...) n)))]
        [?phaseless-spec
         (phaseless-spec #'?phaseless-spec #f)]))

    (define (maybe-shift spec just)
      (syntax-case* spec
        (for-meta for-syntax for-template for-label)
        sym=?
        [(for-meta ?level ?phaseless-spec ...)
         (let ([level (syntax-e #'?level)])
           (each phaseless-spec #'(?phaseless-spec ...)
                 (- just (or level 0))))]
        [(for-syntax ?phaseless-spec ...)
         (each phaseless-spec #'(?phaseless-spec ...) (- just 1))]
        [(for-template ?phaseless-spec ...)
         (each phaseless-spec #'(?phaseless-spec ...) (- just -1))]
        [(for-label ?phaseless-spec ...)
         (each phaseless-spec #'(?phaseless-spec ...) just)]
        [?phaseless-spec
         (phaseless-spec #'?phaseless-spec just)]))

    (syntax-case* spec
      (for-meta for-syntax for-template for-label just-meta)
      sym=?
      [(for-meta ?level ?phaseless-spec ...)
       (each maybe-just-meta #'(?phaseless-spec ...))]
      [(for-syntax ?phaseless-spec ...)
       (each maybe-just-meta #'(?phaseless-spec ...))]
      [(for-template ?phaseless-spec ...)
       (each maybe-just-meta #'(?phaseless-spec ...))]
      [(for-label ?phaseless-spec ...)
       (each maybe-just-meta #'(?phaseless-spec ...))]
      [(just-meta ?level ?raw-require-spec ...)
       (let ([level (syntax-e #'?level)])
         (when level
           (each maybe-shift #'(?raw-require-spec ...) level)))]
      [?phaseless-spec
       (phaseless-spec #'?phaseless-spec #f)]))

  (define (walk form phase)
    (kernel-syntax-case/phase form phase
                              [(module ?id ?path (_ ?form ...))
                               (begin
                                 (phaseless-spec #'?path #f)
                                 (walk* #'(?form ...) 0))]
                              [(module* ?id #f (_ ?form ...))
                               (walk* #'(?form ...) phase)]
                              [(module* ?id ?path (_ ?form ...))
                               (begin
                                 (phaseless-spec #'?path #f)
                                 (walk* #'(?form ...) 0))]
                              [(#%require ?spec ...)
                               (for ([spec (in-syntax #'(?spec ...))])
                                 (raw-require-spec spec))]
                              [(begin ?form ...)
                               (walk* #'(?form ...) phase)]
                              [(begin-for-syntax ?form ...)
                               (walk* #'(?form ...) (add1 phase))]
                              [_ (void)]))

  (define (walk* form* phase)
    (for-each (λ (s) (walk s phase)) (syntax->list form*)))

  (kernel-syntax-case fpe #f
    [(module ?id ?path (#%plain-module-begin ?form ...))
     (begin
       (phaseless-spec #'?path #f)
       (walk* #'(?form ...) (namespace-base-phase))

       (define (get-exports mod just)
         (define (filter-exports exports)
           (cond
             [(not just) (for/union ([p (in-list exports)])
                           (list->set (map car (cdr p))))]
             [(assq just exports)
              =>
              (λ (p)
                (list->set (map car (cdr p))))]
             [else (set)]))
         (let-values ([(a b) (module->exports mod)])
           (set-union (filter-exports a) (filter-exports b))))

       (for ([mod (in-set declared-modules)])
         ((current-module-name-resolver) mod #f #f #t))

       (for ([jm (in-set alls)])
         (for ([id (in-set (get-exports (cdr jm) (car jm)))])
           (set-add! ids id)))
       (for ([jm (in-set prefixs)])
         (for ([id (in-set (get-exports (cadr jm) (car jm)))])
           (set-add! ids (string->symbol (string-append (symbol->string (cddr jm))
                                                        (symbol->string id))))))
       (for ([jm (in-set all-excepts)])
         (define e (foldl (λ (v s) (set-remove s v)) (get-exports (cadr jm) (car jm)) (cddr jm)))
         (for ([id (in-set e)])
           (set-add! ids id)))

       (for ([jm (in-set prefix-all-excepts)])
         (define e (foldl (λ (v s) (set-remove s v)) (get-exports (cadr jm) (car jm)) (cdddr jm)))
         (for ([id (in-set e)])
           (set-add! ids (string->symbol (string-append (symbol->string (caddr jm))
                                                        (symbol->string id)))))))])
  ids)
