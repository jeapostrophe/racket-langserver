#lang racket/base
(require drracket/check-syntax
         racket/class
         racket/contract
         racket/logging
         syntax/modread
         racket/path
         racket/port
         "editor.rkt"
         "doc-trace.rkt"
         (only-in "lexer.rkt"
                  LexerState?
                  LexerState-language-policy
                  Language-Policy-expand?)
         "../common/source.rkt"
         "internal-types.rkt")

;; TODO: cache the namespace with some strategy
(define (expand-source source in collector #:expand? [expand? #t])
  ;; A virtual URI identifies syntax, but supplies no filesystem directory.
  (define file-path (source-file-path source))
  (define src-dir (and file-path (path-only (path->complete-path file-path))))
  (define ns (make-base-namespace))
  (define-values (add-syntax done)
    (make-traversal ns src-dir))
  (port-count-lines! in)

  (parameterize ([current-load-relative-directory src-dir]
                 [current-namespace ns]
                 [current-annotations collector])
    (define stx
      (with-handlers ([exn:fail? (λ (exn) exn)])
        (with-module-reading-parameterization
          (λ () (read-syntax (source-syntax-source source) in)))))

    (define expand-logs '())
    (define expanded-stx
      (with-intercepted-logging
        (λ (log) (set! expand-logs (cons log expand-logs)))
        (λ ()
          (with-handlers ([exn:fail? (λ (exn) exn)])
            (parameterize ([current-output-port (open-output-nowhere)])
              (if (and expand? (syntax? stx))
                  (expand stx)
                  #f))))
        'info))

    (when (syntax? expanded-stx)
      (add-syntax expanded-stx)
      (done))

    ;; Interceptor conses in O(1); reverse restores emission order so overlapping
    ;; Typed Racket provider records apply as sent.
    (ExpandResult stx expanded-stx (reverse expand-logs))))

;; Struct to hold the result of check-syntax.
(struct/contract CSResult
  ([trace (is-a?/c build-trace%)]
   [text string?]
   [succeed? boolean?])
  #:transparent)

(define (check-syntax source doc-text lexer-state)
  (define text (send doc-text get-text))
  (define active-policy (LexerState-language-policy lexer-state))
  (define expand? (Language-Policy-expand? active-policy))
  (define new-trace
    (new build-trace%
      [source source]
      [doc-text doc-text]
      [lexer-state lexer-state]))

  (define in (open-input-string text))
  (define er (expand-source source in new-trace #:expand? expand?))

  (send new-trace walk-stx er)
  (send new-trace walk-log (ExpandResult-logs er))
  (CSResult new-trace text
            (if expand?
                (ExpandResult-all-succeed? er)
                (and (ExpandResult-pre-syntax er) #t))))

(provide
  (struct-out CSResult)
  (contract-out
    [expand-source (->* (Source? input-port? (is-a?/c syncheck-annotations<%>))
                        (#:expand? boolean?)
                        ExpandResult?)]
    [check-syntax (-> Source? (is-a?/c lsp-editor%) LexerState? CSResult?)]))
