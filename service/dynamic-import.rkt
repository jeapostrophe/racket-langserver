#lang racket/base

(provide dynamic-imports)

(define-syntax-rule (import-once mod name fail-thunk)
  (define name
    (let ([logging-fail-thunk (λ ()
                                (log-info "symbol '~a' from module '~a' fail to load." 'name mod)
                                (fail-thunk))])
      (with-handlers ([exn? (λ (e)
                              (logging-fail-thunk)
                              (void))])
        (dynamic-require mod 'name logging-fail-thunk)))))

(define-syntax-rule (dynamic-import-mod mod names ... fail-thunk)
  (begin
    (import-once mod names fail-thunk) ...))

(define-syntax-rule (dynamic-imports (mod names ...) ... fail-thunk)
  (begin
    (dynamic-import-mod mod names ... fail-thunk) ...))

