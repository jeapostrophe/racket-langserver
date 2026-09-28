#lang racket/base

(require rackunit
         racket/async-channel
         racket/file
         racket/list
         racket/set
         racket/string
         "../../common/interfaces.rkt"
         "../../common/path-util.rkt"
         "../../common/settings.rkt"
         "../../doclib/doc.rkt"
         "../../lsp/lsp.rkt"
         "../../lsp/safedoc.rkt"
         (submod "../../lsp/safedoc.rkt" test-support)
         "../../lsp/workspace.rkt"
         "../../workspace/api.rkt"
         "../../workspace/current.rkt"
         "analysis-test-support.rkt")

(define source-text "#lang racket/base\n(require racket/list)\nfirst\n")

(define (notify-watched! uri type)
  (didChangeWatchedFiles
    (hasheq 'changes (list (hasheq 'uri uri 'type type)))))

(define (with-source proc)
  (define directory (make-temporary-file "watched-cache~a" 'directory))
  (define path (build-path directory "source.rkt"))
  (define uri (path->uri path))
  (define alias (string-replace uri "source.rkt" "%73ource.rkt"))
  (define resyntax-enabled (get-resyntax-enabled))
  (dynamic-wind
    (lambda ()
      (display-to-file source-text path)
      (workspace-add-folder! current-workspace directory)
      (set-resyntax-enabled! #f))
    (lambda () (proc path uri alias))
    (lambda ()
      (lsp-close-doc! uri)
      (lsp-close-doc! alias)
      (workspace-remove-folder! current-workspace directory)
      (set-resyntax-enabled! resyntax-enabled)
      (delete-directory/files directory))))

(define (replace-buffer! sd text)
  (with-write-doc sd
    (lambda (doc)
      (doc-reset! doc text)
      (doc-update-version! doc (add1 (Doc-version doc))))))

(define (open-accepted! uri [text source-text])
  (define sd (lsp-open-doc! uri text 2))
  (analyze! sd)
  (when (and (equal? text source-text)
             (file-exists? (uri->path uri)))
    (wait-for-disk-verification! sd))
  (define contribution
    (with-read-doc sd Doc-contribution))
  (values sd (first (hash-keys (Doc-Contribution-references contribution)))))

(define (check-paths binding expected)
  (check-equal?
    (map Reference-Source-path (workspace-reference-sources current-workspace binding))
    expected))

(define (check-locations binding uri lines)
  (check-equal?
    (list->set
      (append-map Reference-Source-locations
                  (workspace-reference-sources current-workspace binding)))
    (for/set ([line (in-list lines)])
      (Location uri (Range (Pos line 0) (Pos line 5))))))

(module+ test
  (test-case
    "closing an already-unsaved buffer discards its accepted contribution"
    (with-source
      (lambda (path uri _alias)
        (define-values (_sd binding)
          (open-accepted! uri (string-append source-text "first\n")))
        (check-paths binding (list path))
        (lsp-close-doc! uri)
        (check-paths binding '())
        (check-equal? (file->string path) source-text))))

  (test-case
    "closing after an unsaved edit discards its accepted contribution"
    (with-source
      (lambda (path uri _alias)
        (define-values (sd binding) (open-accepted! uri))
        (replace-buffer! sd (string-append source-text "first\n"))
        (analyze! sd)
        (check-paths binding (list path))
        (lsp-close-doc! uri)
        (check-paths binding '()))))

  (test-case
    "restoring buffer text alone does not validate an older unsaved contribution"
    (with-source
      (lambda (_path uri _alias)
        (define-values (sd binding)
          (open-accepted! uri (string-append source-text "first\n")))
        (replace-buffer! sd source-text)
        (lsp-close-doc! uri)
        (check-paths binding '()))))

  (test-case
    "a failed unsaved edit retains a previously accepted disk-matching contribution"
    (with-source
      (lambda (path uri _alias)
        (define-values (sd binding) (open-accepted! uri))
        (replace-buffer! sd "#lang racket/base\n(")
        (analyze! sd 'failed)
        (lsp-close-doc! uri)
        (check-paths binding (list path)))))

  (test-case
    "analysis matching newly saved text can be retained on close"
    (with-source
      (lambda (path uri _alias)
        (define text (string-append source-text "first\n"))
        (define-values (sd binding) (open-accepted! uri text))
        (display-to-file text path #:exists 'truncate)
        (analyze! sd)
        (wait-for-disk-verification! sd)
        (lsp-close-doc! uri)
        (check-paths binding (list path)))))

  (test-case
    "missing or unreadable source files do not prevent buffer analysis"
    (for ([directory? '(#f #t)])
      (with-source
        (lambda (path uri _alias)
          (delete-file path)
          (when directory? (make-directory path))
          (define-values (_sd binding) (open-accepted! uri))
          (check-paths binding (list path))
          (lsp-close-doc! uri)
          (check-paths binding '())))))

  (test-case
    "closing either alias restores the survivor regardless of publication order"
    (for* ([unsaved-first? '(#f #t)]
           [saved-last? '(#f #t)])
      (with-source
        (lambda (path uri alias)
          (define-values (saved binding) (open-accepted! uri))
          (define-values (_unsaved _binding)
            (open-accepted! alias (string-append source-text "first\n")))
          (when saved-last?
            (analyze! saved)
            (wait-for-disk-verification! saved))
          (check-locations binding uri (if saved-last? '(2) '(2 3)))
          (lsp-close-doc! (if unsaved-first? alias uri))
          (check-paths binding (list path))
          (check-locations binding uri (if unsaved-first? '(2) '(2 3)))
          (notify-watched! alias 3)
          (check-locations binding uri (if unsaved-first? '(2) '(2 3)))
          (lsp-close-doc! (if unsaved-first? uri alias))
          (check-paths binding '())))))

  (test-case
    "an unanalysed or failed survivor cannot retain a closed alias's facts"
    (for* ([saved? '(#f #t)]
           [failed? '(#f #t)])
      (with-source
        (lambda (_path uri alias)
          (define survivor (lsp-open-doc! alias "#lang racket/base\n(" 2))
          (when failed? (analyze! survivor 'failed))
          (define-values (_owner binding)
            (open-accepted! uri (if saved? source-text
                                    (string-append source-text "first\n"))))
          (lsp-close-doc! uri)
          (check-locations binding uri '())
          ;; A later accepted result can still populate the same path.
          (replace-buffer! survivor source-text)
          (analyze! survivor)
          (check-locations binding uri '(2))))))

  (test-case
    "watched events preserve the current contribution while aliases remain open"
    (with-source
      (lambda (_path uri alias)
        (define-values (saved binding) (open-accepted! uri))
        (define-values (_other _binding)
          (open-accepted! alias (string-append source-text "first\n")))
        (for ([saved-last? '(#f #t)])
          (when saved-last? (analyze! saved))
          (notify-watched! alias 2)
          (check-locations binding uri (if saved-last? '(2) '(2 3)))))))

  (test-case
    "closing verified aliases retains the surviving accepted contribution"
    (for ([alias-first? '(#f #t)])
      (with-source
        (lambda (path uri alias)
          (define-values (_saved binding) (open-accepted! uri))
          (define-values (_other _binding) (open-accepted! alias))
          (lsp-close-doc! (if alias-first? alias uri))
          (check-locations binding uri '(2))
          (lsp-close-doc! (if alias-first? uri alias))
          (check-paths binding (list path))
          (check-locations binding uri '(2))))))

  (test-case
    "closing an unsaved alias does not invalidate a verified survivor"
    (for* ([saved-alias? '(#f #t)] [saved-last? '(#f #t)])
      (with-source
        (lambda (path uri alias)
          (define saved-uri (if saved-alias? alias uri))
          (define unsaved-uri (if saved-alias? uri alias))
          (define-values (saved binding) (open-accepted! saved-uri))
          (open-accepted! unsaved-uri (string-append source-text "first\n"))
          (when saved-last?
            (analyze! saved)
            (wait-for-disk-verification! saved))
          (lsp-close-doc! unsaved-uri)
          (check-locations binding uri '(2))
          (lsp-close-doc! saved-uri)
          (check-paths binding (list path))
          (check-locations binding uri '(2))))))

  (test-case
    "reopening an alias starts without its retired contribution"
    (with-source
      (lambda (_path uri alias)
        (define-values (saved binding) (open-accepted! uri))
        (define-values (retired _binding)
          (open-accepted! alias (string-append source-text "first\n")))
        ;; Failed analysis preserves the survivor's last accepted facts.
        (replace-buffer! saved "#lang racket/base\n(")
        (analyze! saved 'failed)
        (define reopened
          (lsp-open-doc! alias (string-append source-text "first\nfirst\n") 2))
        (check-locations binding uri '(2))
        (check-false
          (with-current-safedoc retired 2
            (lambda (_sd) (fail "retired alias accepted a late publication"))))
        (analyze! reopened)
        (check-locations binding uri '(2 3 4))
        (lsp-close-doc! alias)
        (check-locations binding uri '(2)))))

  (test-case
    "an unanalysed third alias cannot erase an accepted survivor"
    (with-source
      (lambda (_path uri alias)
        (define third (string-replace uri "source.rkt" "s%6furce.rkt"))
        (dynamic-wind
          void
          (lambda ()
            (define-values (_saved binding) (open-accepted! uri))
            (lsp-open-doc! third source-text 2)
            (define-values (_unsaved _binding)
              (open-accepted! alias (string-append source-text "first\n")))
            (lsp-close-doc! alias)
            (check-locations binding uri '(2))
            (lsp-close-doc! uri)
            (check-locations binding uri '()))
          (lambda () (lsp-close-doc! third))))))

  (test-case
    "three divergent aliases never retain a retired contribution in any close order"
    (for ([order (in-list (permutations '(0 1 2)))])
      (with-source
        (lambda (_path uri alias)
          (define third (string-replace uri "source.rkt" "s%6furce.rkt"))
          (define uris (list uri alias third))
          (define expected
            (for/list ([line '(3 4 5)])
              (set (Location uri (Range (Pos 2 0) (Pos 2 5)))
                   (Location uri (Range (Pos line 0) (Pos line 5))))))
          (dynamic-wind
            void
            (lambda ()
              (define binding #f)
              (for ([opened-uri (in-list uris)] [index (in-naturals)])
                (define-values (_sd key)
                  (open-accepted! opened-uri
                                  (string-append source-text (make-string index #\newline)
                                                 "first\n")))
                (set! binding key))
              (let loop ([remaining order])
                (when (pair? remaining)
                  (lsp-close-doc! (list-ref uris (car remaining)))
                  (define locations
                    (list->set
                      (append-map Reference-Source-locations
                                  (workspace-reference-sources current-workspace binding))))
                  (if (null? (cdr remaining))
                      (check-equal? locations (set))
                      (check-not-false
                        (member locations
                                (map (lambda (index) (list-ref expected index))
                                     (cdr remaining)))))
                  (loop (cdr remaining)))))
            (lambda () (lsp-close-doc! third)))))))

  (test-case
    "alias close restores a survivor's concurrent accepted publication"
    (with-source
      (lambda (_path uri alias)
        (define-values (survivor binding) (open-accepted! uri))
        (define-values (_other _binding)
          (open-accepted! alias (string-append source-text "first\n")))
        (replace-buffer! survivor (string-append source-text "first\nfirst\n"))
        (define entered (make-async-channel))
        (define release (make-semaphore))
        (define closing (make-semaphore))
        (define worker #f)
        (define closer #f)
        (dynamic-wind
          void
          (lambda ()
            (safedoc-run-check-syntax!
              (lambda (_method _params)
                (async-channel-put entered (current-thread))
                (sync (semaphore-peek-evt release)))
              survivor)
            (set! worker (sync/timeout 20 entered))
            (check-true (thread? worker))
            (set! closer
                  (thread
                    (lambda ()
                      (semaphore-post closing)
                      (lsp-close-doc! alias))))
            (check-not-false (sync/timeout 5 closing))
            (check-false (sync/timeout 0.05 closer))
            (semaphore-post release)
            (check-eq? (sync/timeout 20 worker) worker)
            (check-eq? (sync/timeout 20 closer) closer)
            (check-locations binding uri '(2 3 4)))
          (lambda ()
            (semaphore-post release)
            (when worker (sync/timeout 20 worker))
            (when closer (sync/timeout 20 closer)))))))

  (test-case
    "reopening after an unsaved close can retain fresh disk-matching analysis"
    (with-source
      (lambda (path uri _alias)
        (define-values (_unsaved binding)
          (open-accepted! uri (string-append source-text "first\n")))
        (lsp-close-doc! uri)
        (check-paths binding '())
        (define-values (_saved _binding) (open-accepted! uri))
        (lsp-close-doc! uri)
        (check-paths binding (list path)))))

  (test-case
    "each disk event invalidates a closed cache without opening a document"
    (for ([type (in-list '(1 2 3))])
      (with-source
        (lambda (path uri alias)
          (define-values (_sd binding) (open-accepted! uri))
          (lsp-close-doc! uri)
          (check-paths binding (list path))
          (notify-watched! alias type)
          (check-paths binding '())
          (check-false (lsp-get-doc uri #f))
          (check-false (lsp-get-doc alias #f))))))

  (test-case
    "each disk event preserves an open alias and invalidates its cache on close"
    (for ([type (in-list '(1 2 3))])
      (with-source
        (lambda (path uri alias)
          (define-values (sd binding) (open-accepted! uri))
          (notify-watched! alias type)
          (check-eq? (lsp-get-doc uri) sd)
          (check-paths binding (list path))
          (lsp-close-doc! uri)
          (check-paths binding '())))))

  (test-case
    "a fresh disk-matching analysis can restore provenance after a disk event"
    (for ([type '(1 2 3)])
      (with-source
        (lambda (path uri alias)
          (define-values (sd binding) (open-accepted! uri))
          (define saved-text (string-append source-text "first\n"))
          (display-to-file saved-text path #:exists 'truncate)
          (notify-watched! alias type)
          (replace-buffer! sd saved-text)
          (analyze! sd)
          (wait-for-disk-verification! sd)
          (lsp-close-doc! uri)
          (check-paths binding (list path))
          (check-locations binding uri '(2 3))))))

  (test-case
    "failed or disk-mismatching analysis cannot restore provenance after an event"
    (for ([failed? '(#f #t)])
      (with-source
        (lambda (_path uri alias)
          (define-values (sd binding) (open-accepted! uri))
          (notify-watched! alias 2)
          (replace-buffer! sd (if failed? "#lang racket/base\n("
                                  (string-append source-text "first\n")))
          (analyze! sd (if failed? 'failed 'succeeded))
          (lsp-close-doc! uri)
          (check-paths binding '())))))

  (test-case
    "invalidation drops closed facts when an open alias has no accepted analysis"
    (with-source
      (lambda (path uri alias)
        (define-values (_sd binding) (open-accepted! uri))
        (define other (lsp-open-doc! alias source-text 2))
        (notify-watched! uri 3)
        (lsp-close-doc! uri)
        (check-eq? (lsp-get-doc alias) other)
        (check-paths binding '())
        (lsp-close-doc! alias)
        (check-paths binding '()))))

  (test-case
    "closing and reopening starts a new document lifetime"
    (with-source
      (lambda (path uri _alias)
        (define-values (old binding) (open-accepted! uri))
        (notify-watched! uri 2)
        (lsp-close-doc! uri)
        (check-paths binding '())
        (define-values (reopened _binding) (open-accepted! uri))
        ;; Simulate a publication already computed for the retired lifetime.
        ;; The same version in a new document must not make it admissible.
        (check-false
          (with-current-safedoc old 2
            (lambda (_sd) (fail "retired document accepted a late publication"))))
        (check-true
          (with-current-safedoc reopened 2 void))
        (check-false
          (with-current-safedoc reopened 1
            (lambda (_sd) (fail "stale version accepted a publication"))))
        (lsp-close-doc! uri)
        (check-paths binding (list path)))))

  (test-case
    "invalid batches cannot mark an open cache dirty or purge a closed cache"
    (with-source
      (lambda (path uri _alias)
        (define-values (_sd binding) (open-accepted! uri))
        (define (invalid-event!)
          (didChangeWatchedFiles
            (hasheq 'changes (list (hasheq 'uri uri 'type 3)
                                   (hasheq 'uri uri 'type 9)))))
        (check-exn exn:fail? invalid-event!)
        (lsp-close-doc! uri)
        (check-paths binding (list path))
        (check-exn exn:fail? invalid-event!)
        (check-paths binding (list path)))))

  (test-case
    "file events are handled even when virtual URIs occur in the same batch"
    (with-source
      (lambda (path uri _alias)
        (define-values (_sd binding) (open-accepted! uri))
        (lsp-close-doc! uri)
        (didChangeWatchedFiles
          (hasheq 'changes
                  (list (hasheq 'uri "untitled:source.rkt" 'type 1)
                        (hasheq 'uri "vscode-remote://example/source.rkt" 'type 2)
                        (hasheq 'uri uri 'type 3))))
        (check-paths binding '()))))

  (test-case
    "an encoded URI owned by the editor is protected from a canonical disk event"
    (with-source
      (lambda (path uri alias)
        (define-values (sd binding) (open-accepted! alias))
        (notify-watched! uri 3)
        (check-eq? (lsp-get-doc alias) sd)
        (check-paths binding (list path))
        (lsp-close-doc! alias)
        (check-paths binding '()))))

  (test-case
    "close serializes with an in-flight publication before purging deleted facts"
    (with-source
      (lambda (path uri alias)
        (define-values (sd binding) (open-accepted! uri))
        (define entered (make-async-channel))
        (define release (make-semaphore))
        (define closing (make-semaphore))
        (define worker #f)
        (define closer #f)
        (dynamic-wind
          void
          (lambda ()
            (safedoc-run-check-syntax!
              (lambda (_method _params)
                (async-channel-put entered (current-thread))
                (semaphore-wait release))
              sd)
            (set! worker (sync/timeout 20 entered))
            (check-true (thread? worker))
            (set! closer
                  (thread
                    (lambda ()
                      (semaphore-post closing)
                      (lsp-close-doc! uri)
                      (notify-watched! alias 3))))
            (check-not-false (sync/timeout 5 closing))
            ;; The callback is inside the existing write critical section.
            ;; Close must wait for it, then retire the document before purging.
            (check-false (sync/timeout 0.05 closer))
            (semaphore-post release)
            (check-eq? (sync/timeout 20 worker) worker)
            (check-eq? (sync/timeout 20 closer) closer)
            (check-false (lsp-get-doc uri #f))
            (check-paths binding '()))
          (lambda ()
            (semaphore-post release)
            (when worker (sync/timeout 20 worker))
            (when closer (sync/timeout 20 closer))))))))
