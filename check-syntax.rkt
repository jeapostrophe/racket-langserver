#lang racket/base
(require drracket/check-syntax
         racket/class
         racket/contract
         racket/logging
         syntax/modread
         racket/port
         "editor.rkt"
         "doc-trace.rkt"
         "path-util.rkt"
         "internal-types.rkt")

(define (get-indenter text)
  (define lang-info
    (with-handlers ([exn:fail:read? (lambda (e) 'missing)]
                    [exn:missing-module? (lambda (e) #f)])
      (read-language (open-input-string text) (lambda () 'missing))))
  (cond
    [(procedure? lang-info)
     (lang-info 'drracket:indentation #f)]
    [(eq? lang-info 'missing)
     ; check for a #reader directive at start of file, ignoring comments
     ; the ^ anchor here matches start-of-string, not start-of-line
     (if (regexp-match #rx"^(;[^\n]*\n)*#reader" text)
         #f ; most likely a drracket file, use default indentation
         ; (https://github.com/jeapostrophe/racket-langserver/issues/86)
         'missing)]
    [else #f]))

;; TODO: cache the namespace with some strategy
(define (expand-source path in collector)
  (define-values (src-dir _1 _2) (split-path path))
  (define ns (make-base-namespace))
  (define-values (add-syntax done)
    (make-traversal ns src-dir))
  (port-count-lines! in)

  (parameterize ([current-load-relative-directory src-dir]
                 [current-namespace ns]
                 [current-annotations collector])
    (define stx
      (with-handlers ([(λ _ #t) (λ (exn) exn)])
        (with-module-reading-parameterization
          (λ () (read-syntax path in)))))

    (define expand-logs '())
    (define expanded-stx
      (with-intercepted-logging
        (λ (log) (set! expand-logs (cons log expand-logs)))
        (λ ()
          (with-handlers ([(λ _ #t) (λ (exn) exn)])
            (parameterize ([current-output-port (open-output-nowhere)])
              (if (syntax? stx)
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
  (define indenter (get-indenter text))
  (define new-trace (new build-trace% [src path] [doc-text doc-text] [indenter indenter]))

  (define in (open-input-string text))
  (define er (expand-source path in new-trace))

  (define pre-stx (ExpandResult-pre-syntax er))
  (define post-stx (ExpandResult-post-syntax er))

  (send new-trace walk-stx er)
  (send new-trace walk-log (ExpandResult-logs er))
  (CSResult new-trace text (and pre-stx post-stx)))

(provide
  (struct-out CSResult)
  (contract-out
    [expand-source (-> path? input-port? (is-a?/c syncheck-annotations<%>) ExpandResult?)]
    [check-syntax (-> any/c (is-a?/c lsp-editor%) CSResult?)]))

