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
         "doc-lang.rkt"
         "../common/path-util.rkt"
         "internal-types.rkt")

;; TODO: cache the namespace with some strategy
(define (expand-source path in collector #:expand? [expand? #t])
  (define-values (src-dir _1 _2) (split-path path))
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
          (λ () (read-syntax path in)))))

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

    (ExpandResult stx expanded-stx expand-logs)))

;; Struct to hold the result of check-syntax.
(struct/contract CSResult
  ([trace (is-a?/c build-trace%)]
   [text string?]
   [succeed? boolean?])
  #:transparent)

(define (check-syntax uri doc-text)
  (define path (uri->path uri))
  (define text (send doc-text get-text))
  (define expand? (requires-expansion? path))
  (define new-trace (new build-trace% [src path] [doc-text doc-text]))

  (define in (open-input-string text))
  (define er (expand-source path in new-trace #:expand? expand?))

  (send new-trace walk-stx er)
  (send new-trace walk-log (ExpandResult-logs er))
  (CSResult new-trace text
            (if expand?
                (ExpandResult-all-succeed? er)
                (and (ExpandResult-pre-syntax er) #t))))

(provide
  (struct-out CSResult)
  (contract-out
    [expand-source (->* (path? input-port? (is-a?/c syncheck-annotations<%>))
                        (#:expand? boolean?)
                        ExpandResult?)]
    [check-syntax (-> string? (is-a?/c lsp-editor%) CSResult?)]))
