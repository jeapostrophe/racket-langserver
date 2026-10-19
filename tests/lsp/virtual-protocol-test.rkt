#lang racket/base

(require "../../common/source.rkt"
         rackunit
         racket/async-channel
         "../../common/interfaces.rkt"
         "../../common/json-util.rkt"
         "../../common/path-util.rkt"
         "../../common/settings.rkt"
         "../../lsp/analysis.rkt"
         "../../lsp/source-controller.rkt"
         "../../workspace/source-store.rkt"
         (submod "source-controller-test.rkt" test-support))

(struct Probe (snapshot release) #:transparent)

(define text "#lang racket/base\n(define value 1)\nvalue\n")
(define uri "untitled:Untitled-1")
(define source (uri->source uri))

(define (open-uri! session [document-text text])
  (notify session "textDocument/didOpen"
          (hasheq 'textDocument
                  (hasheq 'uri uri 'languageId "racket" 'version 0 'text document-text))))

(define (close-uri! session)
  (notify session "textDocument/didClose" (hasheq 'textDocument (hasheq 'uri uri))))

(define (request! session request-id method [extra (hasheq)])
  (async-channel-put (Session-clients session)
                     (hasheq 'id request-id 'method method
                             'params (hash-set extra 'textDocument (hasheq 'uri uri)))))

(define (check-error result code)
  (check-equal? (hash-ref (hash-ref result 'error) 'code) (->jsexpr code)))

(define (analysis-diagnostics session)
  (hash-ref (hash-ref (await (Session-notifications session)) 'params) 'diagnostics))

(module+ test
  (test-case "virtual edits and close cancel exact requests through the LSP consumer"
    (define probes (make-async-channel))
    (with-session
      (make-source-controller
        #:resyntax #f
        #:analyze (lambda (snapshot)
                    (define release (make-async-channel))
                    (async-channel-put probes (Probe snapshot release))
                    (async-channel-get release)
                    (analyze-source snapshot)))
      (lambda (session)
        (open-uri! session)
        (define initial (await probes))
        (define range (hasheq 'range (hasheq 'start (hasheq 'line 0 'character 0)
                                             'end (hasheq 'line 3 'character 0))))
        (request! session 1 "textDocument/inlayHint" range)
        (request! session 2 "textDocument/semanticTokens/full")
        (barrier session)
        (notify session "textDocument/didChange"
                (hasheq 'textDocument (hasheq 'uri uri 'version 1)
                        'contentChanges
                        (list (hasheq 'range (hasheq 'start (hasheq 'line 1 'character 14)
                                                     'end (hasheq 'line 1 'character 15))
                                      'text "2"))))
        (define edited (await probes))
        (check-equal? (Source-Snapshot-id (Probe-snapshot initial))
                      (Source-Snapshot-id (Probe-snapshot edited)))
        (check-equal? (Source-Snapshot-text (Probe-snapshot edited))
                      "#lang racket/base\n(define value 2)\nvalue\n")
        (check-error (response session 1) ErrorCode-RequestCancelled)
        (check-error (response session 2) ErrorCode-RequestCancelled)
        (request! session 3 "textDocument/inlayHint" range)
        (request! session 4 "textDocument/semanticTokens/full")
        (barrier session)
        (check-false (sync/timeout 0 (Session-responses session)))
        (async-channel-put (Probe-release edited) 'go)
        (check-true (hash-has-key? (response session 3) 'result))
        (check-not-equal? (hash-ref (hash-ref (response session 4) 'result) 'data) '())
        (analysis-diagnostics session)
        ;; A full replacement starts another revision, then close cancels it.
        (notify session "textDocument/didChange"
                (hasheq 'textDocument (hasheq 'uri uri 'version 2)
                        'contentChanges (list (hasheq 'text text))))
        (await probes)
        (request! session 5 "textDocument/semanticTokens/full")
        (barrier session)
        (close-uri! session)
        (barrier session)
        (check-error (response session 5) ErrorCode-RequestCancelled)
        (check-equal? (analysis-diagnostics session) '())
        (open-uri! session)
        (define reopened (await probes))
        (check-equal? (Source-Snapshot-id (Probe-snapshot initial))
                      (Source-Snapshot-id (Probe-snapshot reopened)))
        (check-not-eq? (Source-Snapshot-revision (Probe-snapshot initial))
                       (Source-Snapshot-revision (Probe-snapshot reopened)))
        (async-channel-put (Probe-release reopened) 'go)
        (check-eq? 'succeeded (Analysis-Outcome-state (await (acquire session source))))
        (check-equal? (Source-Snapshot-uri (Probe-snapshot reopened)) uri)
        (close-uri! session)
        (barrier session))))

  (test-case "headerless virtual text keeps language rules and recovers after a full change"
    (with-session (make-source-controller #:resyntax #f)
                  (lambda (session)
                    (open-uri! session "(define value 1)\nvalue\n")
                    (barrier session)
                    (await (acquire session source))
                    (define diagnostics (analysis-diagnostics session))
                    (check-not-false
                      (for/or ([diag (in-list diagnostics)])
                        (regexp-match? #rx"Missing language header" (hash-ref diag 'message))))
                    (notify session "textDocument/didChange"
                            (hasheq 'textDocument (hasheq 'uri uri 'version 1)
                                    'contentChanges (list (hasheq 'text text))))
                    (barrier session)
                    (check-eq? 'succeeded (Analysis-Outcome-state (await (acquire session source))))
                    (check-equal? (analysis-diagnostics session) '())
                    (request! session 1 "textDocument/definition"
                              (hasheq 'position (hasheq 'line 2 'character 0)))
                    (check-equal? (hash-ref (hash-ref (response session 1) 'result) 'uri) uri)
                    (close-uri! session)
                    (barrier session))))

  (test-case "virtual Resyntax receives client text and publishes original-URI edits"
    (define old-enabled (get-resyntax-enabled))
    (dynamic-wind
      (lambda () (set-resyntax-enabled! #t))
      (lambda ()
        (define starts (make-async-channel))
        (with-session
          (make-source-controller
            #:resyntax (lambda (source uri)
                         (async-channel-put starts (list source uri))
                         (list (Resyntax-Result 18 23 "example suggestion" 'example "renamed"))))
          (lambda (session)
            (open-uri! session)
            (barrier session)
            (check-eq? 'succeeded (Analysis-Outcome-state (await (acquire session source))))
            (check-equal? (await starts) (list text uri))
            (analysis-diagnostics session) ; Check Syntax publication
            (check-equal? (length (analysis-diagnostics session)) 1) ; Resyntax publication
            (request! session 1 "textDocument/codeAction"
                      (hasheq 'range (hasheq 'start (hasheq 'line 0 'character 0)
                                             'end (hasheq 'line 3 'character 0))
                              'context (hasheq 'diagnostics '())))
            (define actions (hash-ref (response session 1) 'result))
            (check-equal? (length actions) 1)
            (check-equal? (hash-keys (hash-ref (hash-ref (car actions) 'edit) 'changes))
                          (list (string->symbol uri)))
            (close-uri! session)
            (barrier session))))
      (lambda () (set-resyntax-enabled! old-enabled)))))
