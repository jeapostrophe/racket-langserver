#lang racket/base

(module+ test
  (require "controller-support.rkt"
           "../json-dsl.rkt"
           rackunit
           racket/async-channel
           "../../common/interfaces.rkt"
           "../../common/json-util.rkt"
           "../../common/settings.rkt"
           "../../common/source.rkt"
           "../../lsp/analysis.rkt"
           "../../lsp/source-controller.rkt"
           "../../workspace/source-store.rkt")

  (define text "#lang racket/base\n(define value 1)\nvalue\n")
  (define uri "untitled:Untitled-1")
  (define source (uri->source uri))

  (define (analysis-diagnostics session)
    (hash-ref (hash-ref (await (Session-notifications session)) 'params) 'diagnostics))

  (test-case "virtual edits and close cancel exact requests through the LSP consumer"
    (define probes (make-async-channel))
    (with-session
      (make-source-controller #:resyntax #f #:analyze (controlled-analyzer probes))
      (lambda (session)
        (open! session uri text)
        (define initial (await probes))
        (define range (json {#:range (->jsexpr (Range (Pos 0 0) (Pos 3 0)))}))
        (request! session 1 "textDocument/inlayHint" uri range)
        (request! session 2 "textDocument/semanticTokens/full" uri)
        (barrier session)
        (notify session "textDocument/didChange"
                (json {#:textDocument (->jsexpr (DocIdentifier 1 uri))
                       #:contentChanges [(->jsexpr
                                           (ContentChangeEvent-Incremental
                                             (Range (Pos 1 14) (Pos 1 15))
                                             (Nothing)
                                             "2"))]}))
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
                              (json {#:position (->jsexpr (Pos 2 0))}))
                    (check-equal? (hash-ref (hash-ref (response session 1) 'result) 'uri) uri)
                    (close! session uri)
                    (barrier session))))

  (test-case "virtual Resyntax receives client text and publishes original-URI edits"
    (call-with-resyntax-enabled
      #t
      (lambda ()
        (define starts (make-async-channel))
        (with-session
          (make-source-controller
            #:resyntax (lambda (source-text doc-uri)
                         (async-channel-put starts (list source-text doc-uri))
                         (list (Resyntax-Result 18 23 "example suggestion" 'example "renamed"))))
          (lambda (session)
            (open! session uri text)
            (barrier session)
            (check-eq? 'succeeded (Analysis-Outcome-state (await (acquire session source))))
            (check-equal? (await starts) (list text uri))
            (analysis-diagnostics session)
            (check-equal? (length (analysis-diagnostics session)) 1)
            (request! session 1 "textDocument/codeAction" uri
                      (json {#:range (->jsexpr (Range (Pos 0 0) (Pos 3 0)))
                             #:context {#:diagnostics []}}))
            (define actions (hash-ref (response session 1) 'result))
            (check-equal? (length actions) 1)
            (check-equal? (hash-keys (hash-ref (hash-ref (car actions) 'edit) 'changes))
                          (list (string->symbol uri)))
            (close! session uri)
            (barrier session))))))

  (test-case "closed virtual feature requests receive RequestFailed"
    (with-session (make-source-controller #:resyntax #f)
                  (lambda (session)
                    (request! session 1 "textDocument/hover" "untitled:Closed"
                              (json {#:position (->jsexpr (Pos 0 0))}))
                    (check-error (response session 1) ErrorCode-RequestFailed)))))
