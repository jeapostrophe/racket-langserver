#lang racket/base

(require "../../common/source.rkt"
         rackunit
         racket/async-channel
         "../../common/interfaces.rkt"
         "../../common/settings.rkt"
         "../../lsp/analysis.rkt"
         "../../lsp/source-controller.rkt"
         "../../workspace/source-store.rkt"
         "controller-support.rkt")

(define text "#lang racket/base\n(define value 1)\nvalue\n")
(define uri "untitled:Untitled-1")
(define source (uri->source uri))

(define (analysis-diagnostics session)
  (hash-ref (hash-ref (await (Session-notifications session)) 'params) 'diagnostics))

(module+ test
  (test-case "virtual edits and close cancel exact requests through the LSP consumer"
    (define probes (make-async-channel))
    (with-session
      (make-source-controller
        #:resyntax #f
        #:analyze (lambda (snapshot)
                    (wait-at-gate probes snapshot)
                    (analyze-source snapshot)))
      (lambda (session)
        (open! session uri text)
        (define initial (await probes))
        (define range (hasheq 'range (hasheq 'start (hasheq 'line 0 'character 0)
                                             'end (hasheq 'line 3 'character 0))))
        (request! session 1 "textDocument/inlayHint" uri range)
        (request! session 2 "textDocument/semanticTokens/full" uri)
        (barrier session)
        (notify session "textDocument/didChange"
                (hasheq 'textDocument (hasheq 'uri uri 'version 1)
                        'contentChanges
                        (list (hasheq 'range (hasheq 'start (hasheq 'line 1 'character 14)
                                                     'end (hasheq 'line 1 'character 15))
                                      'text "2"))))
        (define edited (await probes))
        (check-equal? (Source-Snapshot-id (Gate-input initial))
                      (Source-Snapshot-id (Gate-input edited)))
        (check-equal? (Source-Snapshot-text (Gate-input edited))
                      "#lang racket/base\n(define value 2)\nvalue\n")
        (check-error (response session 1) ErrorCode-ContentModified)
        (check-error (response session 2) ErrorCode-ContentModified)
        (request! session 3 "textDocument/inlayHint" uri range)
        (request! session 4 "textDocument/semanticTokens/full" uri)
        (barrier session)
        (check-false (sync/timeout 0 (Session-responses session)))
        (release-gate! edited)
        (check-true (hash-has-key? (response session 3) 'result))
        (check-not-equal? (hash-ref (hash-ref (response session 4) 'result) 'data) '())
        (analysis-diagnostics session)
        ;; A full replacement starts another revision, then close cancels it.
        (change! session uri text 2)
        (await probes)
        (request! session 5 "textDocument/semanticTokens/full" uri)
        (barrier session)
        (close! session uri)
        (barrier session)
        (check-error (response session 5) ErrorCode-ContentModified)
        (check-equal? (analysis-diagnostics session) '())
        (open! session uri text)
        (define reopened (await probes))
        (check-equal? (Source-Snapshot-id (Gate-input initial))
                      (Source-Snapshot-id (Gate-input reopened)))
        (check-not-eq? (Source-Snapshot-revision (Gate-input initial))
                       (Source-Snapshot-revision (Gate-input reopened)))
        (release-gate! reopened)
        (check-eq? 'succeeded (Analysis-Outcome-state (await (acquire session source))))
        (check-equal? (Source-Snapshot-uri (Gate-input reopened)) uri)
        (close! session uri)
        (barrier session))))

  (test-case "headerless virtual text keeps language rules and recovers after a full change"
    (with-session (make-source-controller #:resyntax #f)
                  (lambda (session)
                    (open! session uri "(define value 1)\nvalue\n")
                    (barrier session)
                    (await (acquire session source))
                    (define diagnostics (analysis-diagnostics session))
                    (check-not-false
                      (for/or ([diag (in-list diagnostics)])
                        (regexp-match? #rx"Missing language header" (hash-ref diag 'message))))
                    (change! session uri text 1)
                    (barrier session)
                    (check-eq? 'succeeded (Analysis-Outcome-state (await (acquire session source))))
                    (check-equal? (analysis-diagnostics session) '())
                    (request! session 1 "textDocument/definition" uri
                              (hasheq 'position (hasheq 'line 2 'character 0)))
                    (check-equal? (hash-ref (hash-ref (response session 1) 'result) 'uri) uri)
                    (close! session uri)
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
            (open! session uri text)
            (barrier session)
            (check-eq? 'succeeded (Analysis-Outcome-state (await (acquire session source))))
            (check-equal? (await starts) (list text uri))
            (analysis-diagnostics session) ; Check Syntax publication
            (check-equal? (length (analysis-diagnostics session)) 1) ; Resyntax publication
            (request! session 1 "textDocument/codeAction" uri
                      (hasheq 'range (hasheq 'start (hasheq 'line 0 'character 0)
                                             'end (hasheq 'line 3 'character 0))
                              'context (hasheq 'diagnostics '())))
            (define actions (hash-ref (response session 1) 'result))
            (check-equal? (length actions) 1)
            (check-equal? (hash-keys (hash-ref (hash-ref (car actions) 'edit) 'changes))
                          (list (string->symbol uri)))
            (close! session uri)
            (barrier session))))
      (lambda () (set-resyntax-enabled! old-enabled)))))

(module+ test
  (test-case "closed virtual feature requests receive RequestFailed"
    (with-session (make-source-controller #:resyntax #f)
                  (lambda (session)
                    (request! session 1 "textDocument/hover" "untitled:Closed"
                              (hasheq 'position (hasheq 'line 0 'character 0)))
                    (check-error (response session 1) ErrorCode-RequestFailed)))))
