#lang racket/base

(require racket/match
         racket/async-channel
         syntax/modread
         syntax/parse
         drracket/check-syntax
         racket/contract
         racket/class)

(provide
  (contract-out
    [read-and-expand (-> input-port?
                         complete-path?
                         (is-a?/c syncheck-annotations<%>)
                         async-channel?)]))

(define *expander-ch* (make-async-channel))

;; (-> input-port path collector% async-channel)
;; async-channel receives (list (or syntax #f) (or expand-syntax #f))
;; where `syntax` is the result of read-syntax,
;; `expand-syntax` is the result of expand.
;; They can be #f if error happens during processing.
(define (read-and-expand in path collector)
  (define ch (make-async-channel))
  (async-channel-put *expander-ch* (list path in ch collector))
  ch)

(define (expander)
  ;; cache namespace for continuous expand request for same uri
  (define ns (make-base-namespace))
  (define last-path #f)

  (let loop ()
    (match-define (list path in out-ch collector) (sync *expander-ch*))

    (define result
      (cond [(equal? last-path path)
             (real-expand path in ns collector)]
            [else
             (set! ns (make-base-namespace))
             (real-expand path in ns collector)]))

    (set! last-path path)

    (async-channel-put out-ch result)
    (loop)))

(define (real-expand path in ns collector)
  (define-values (src-dir _1 _2) (split-path path))
  (define-values (add-syntax done)
    (make-traversal ns src-dir))

  (parameterize ([current-load-relative-directory src-dir]
                 [current-namespace ns]
                 [current-annotations collector])
    (define stx
      (with-handlers ([(λ _ #t) (λ (exn) exn)])
        (with-module-reading-parameterization
          (lambda () (read-syntax path in)))))
    (define expanded
      (with-handlers ([(λ _ #t) (λ (exn) exn)])
        (with-handlers ([(λ _ #t) (λ _ (expand stx))])
          (expand (simplify-stx stx)))))

    (when (not (exn? expanded))
      (add-syntax expanded)
      (done))

    (list stx expanded)))

(define _expand-th (thread expander))

;; simplify syntax to optimize expand
;; for example, use typed/racket/no-check language to avoid
;; type check.
(define (simplify-stx stx)
  (define (apply-rules modname)
    (match modname
      [(or 'typed/racket/base
           'typed/racket/base/deep
           'typed/racket/base/shallow
           'typed/racket/base/optional)
       'typed/racket/base/no-check]
      [(or 'typed/racket
           'typed/racket/deep
           'typed/racket/shallow
           'typed/racket/optional)
       'typed/racket/no-check]
      [_ modname]))

  (define (convert name-stx)
    (define name-sym (syntax->datum name-stx))
    (if (not (symbol? name-sym))
        name-stx
        (datum->syntax name-stx (apply-rules name-sym))))

  (syntax-parse stx
    [(module id mod-path form ...)
     (define new-mod (convert #'mod-path))
     #`(module id #,new-mod form ...)]
    [stx #'stx]))

