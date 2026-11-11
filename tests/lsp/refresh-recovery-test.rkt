#lang racket/base

;; Drive the real protocol dispatcher with held analysis and one outbound queue.
;; Explicitly dispatching consumer events makes response/refresh order observable.
(require rackunit
         json
         racket/async-channel
         racket/class
         racket/match
         "../../common/debug.rkt"
         "../../common/interfaces.rkt"
         "../../common/settings.rkt"
         "../../lsp/analysis.rkt"
         "../../lsp/analysis-query.rkt"
         "../../lsp/methods.rkt"
         "../../lsp/source-controller.rkt"
         (submod "../../lsp/source-controller.rkt" owner)
         "controller-support.rkt")

(define uri "untitled:refresh-recovery")
(define text "#lang racket/base\n(struct point (x y))\n(point 1 2)\n")
(define semantic-tokens-refresh-method "workspace/semanticTokens/refresh")
(define inlay-hint-refresh-method "workspace/inlayHint/refresh")
(define refresh-methods (list semantic-tokens-refresh-method inlay-hint-refresh-method))

(define (with-server capabilities proc)
  (define gates (make-async-channel))
  (define wire (make-async-channel))
  (define controller
    (make-source-controller
      #:resyntax #f
      #:analyze (lambda (snapshot)
                  (wait-at-gate gates snapshot)
                  (analyze-source snapshot))))
  (define server
    (new server% [source-controller controller]
      [response-channel wire] [request-channel wire] [notification-channel wire]))
  (dynamic-wind
    void
    (lambda ()
      (send server process-message
            (hasheq 'id "init" 'method "initialize"
                    'params (hasheq 'processId (json-null) 'rootUri (json-null)
                                    'rootPath (json-null) 'capabilities capabilities)))
      (check-equal? (hash-ref (await wire) 'id) "init")
      (proc server controller gates wire))
    (lambda () (source-controller-shutdown! controller))))

(define (capabilities features)
  (hasheq 'workspace
          (for/hasheq ([feature (in-list features)])
            (values feature (hasheq 'refreshSupport #t)))))

(define (open-buffer! server)
  (send server process-message
        (hasheq 'method "textDocument/didOpen"
                'params (hasheq 'textDocument
                                (hasheq 'uri uri 'languageId "racket" 'version 0 'text text)))))

(define (capture-query server controller id feature)
  (parameterize ([current-analysis-request-timeout 0])
    (send server process-message
          (hasheq 'id id
                  'method (if (eq? feature 'semantic-tokens)
                              "textDocument/semanticTokens/full" "textDocument/inlayHint")
                  'params (hasheq 'textDocument (hasheq 'uri uri)
                                  'range (hasheq 'start (hasheq 'line 0 'character 0)
                                                 'end (hasheq 'line 50 'character 0))))))
  (define delivery
    (let loop ()
      (define event (await (source-controller-events controller)))
      (cond
        [(Visual-Delivery? event) event]
        [else
         (send server process-source-event event)
         (loop)])))
  (check-true (Visual-Delivery? delivery))
  delivery)

(define (accept! server controller gate wire)
  (release-gate! gate)
  (send server process-source-event (await (source-controller-events controller) 90))
  (check-equal? (hash-ref (await wire) 'method) "textDocument/publishDiagnostics"))

(define (take-refresh! wire method)
  (define request (await wire))
  (check-equal? (hash-ref request 'jsonrpc) "2.0")
  (check-equal? (hash-ref request 'method) method)
  (check-equal? (hash-ref request 'params) (hasheq))
  request)

(define (reply! server request [error #f])
  (send server process-message
        (hash-set (hasheq 'jsonrpc "2.0" 'id (hash-ref request 'id))
                  (if error 'error 'result) (or error (json-null)))))

(define (edit! server version [changes (list (hasheq 'text text))])
  (send server process-message
        (hasheq 'method "textDocument/didChange"
                'params (hasheq 'textDocument (hasheq 'uri uri 'version version)
                                'contentChanges changes))))

(module+ test
  (test-case "degraded visuals recover, overlapping refresh replies stay independent"
    (with-server
      (capabilities '(semanticTokens inlayHint))
      (lambda (server controller gates wire)
        (open-buffer! server)
        (define first (await gates))
        (define fallback (capture-query server controller 1 'inlay-hints))
        (check-true (Visual-Delivery-degraded? fallback))
        (send server process-source-event fallback)
        (check-equal? (hash-ref (await wire) 'result) '())
        (check-false (sync/timeout 0 wire))
        (accept! server controller first wire)
        (define tokens (take-refresh! wire semantic-tokens-refresh-method))
        (define hints (take-refresh! wire inlay-hint-refresh-method))
        (define fresh (capture-query server controller 2 'inlay-hints))
        (check-false (Visual-Delivery-degraded? fresh))
        (send server process-source-event fresh)
        (define fresh-hints (hash-ref (await wire) 'result))
        (check-not-equal? fresh-hints '())
        (check-false (sync/timeout 0 wire))
        ;; An unrelated incomplete edit preserves hints and sends no refresh on failure.
        (edit! server 1
               (list (hasheq 'range (hasheq 'start (hasheq 'line 3 'character 0)
                                            'end (hasheq 'line 3 'character 0))
                             'text "(define")))
        (accept! server controller (await gates) wire)
        (check-false (sync/timeout 0 wire))
        (send server process-source-event (capture-query server controller 3 'inlay-hints))
        (check-equal? (hash-ref (await wire) 'result) fresh-hints)
        (check-false (sync/timeout 0 wire))
        (edit! server 2)
        (accept! server controller (await gates) wire)
        (define newer-tokens (take-refresh! wire semantic-tokens-refresh-method))
        (define newer-hints (take-refresh! wire inlay-hint-refresh-method))
        ;; No in-flight lock: the second success refreshes before the first replies.
        (check-not-equal? (hash-ref tokens 'id) (hash-ref newer-tokens 'id))
        (reply! server newer-hints)
        (reply! server tokens (hasheq 'code -32601 'message "unsupported"))
        (reply! server hints)
        (reply! server newer-tokens)
        (reply! server tokens) ; duplicate/unknown reply does not generate a response
        (check-false (sync/timeout 0 wire)))))

  (test-case "success before delivery queues fallback then its feature refresh; stale delivery cancels"
    (with-server
      (capabilities '(semanticTokens inlayHint))
      (lambda (server controller gates wire)
        (open-buffer! server)
        (define first (await gates))
        (define tokens (capture-query server controller 10 'semantic-tokens))
        (define hints (capture-query server controller 11 'inlay-hints))
        (accept! server controller first wire)
        (for ([method (in-list refresh-methods)]) (take-refresh! wire method))
        (for ([delivery (in-list (list tokens hints))]
              [method (in-list refresh-methods)])
          (send server process-source-event delivery)
          (check-equal? (await wire) (Visual-Delivery-response delivery))
          (take-refresh! wire method)
          (check-false (sync/timeout 0 wire)))
        ;; Edit invalidates the captured view even when its exact analysis was ready.
        (define pre-edit (capture-query server controller 12 'semantic-tokens))
        (edit! server 1)
        (send server process-source-event pre-edit)
        (check-error (await wire) ErrorCode-ContentModified)
        (await gates)
        (define closed (capture-query server controller 13 'semantic-tokens))
        (send server process-message
              (hasheq 'method "textDocument/didClose"
                      'params (hasheq 'textDocument (hasheq 'uri uri))))
        (check-equal? (hash-ref (await wire) 'method) "textDocument/publishDiagnostics")
        (send server process-source-event closed)
        (check-error (await wire) ErrorCode-ContentModified)
        (check-false (sync/timeout 0 wire))
        ;; Shutdown forbids recovery even for a still-current captured view.
        (open-buffer! server)
        (define last (await gates))
        (define stopped (capture-query server controller 14 'inlay-hints))
        (release-gate! last)
        (define late-success (await (source-controller-events controller) 90))
        (source-controller-shutdown! controller)
        (send server process-source-event late-success)
        (send server process-source-event stopped)
        (check-error (await wire) ErrorCode-RequestFailed)
        (check-false (sync/timeout 0 wire)))))

  (test-case "refresh capabilities gate each feature independently"
    (for ([feature '(semantic-tokens inlay-hints)]
          [capability '(semanticTokens inlayHint)])
      (define method (if (eq? feature 'semantic-tokens)
                         semantic-tokens-refresh-method
                         inlay-hint-refresh-method))
      ;; Matching capability enables refresh on acceptance and delayed fallback.
      (with-server
        (capabilities (list capability))
        (lambda (server controller gates wire)
          (open-buffer! server)
          (define gate (await gates))
          (define fallback (capture-query server controller 20 feature))
          (accept! server controller gate wire)
          (take-refresh! wire method)
          (send server process-source-event fallback)
          (check-equal? (hash-ref (await wire) 'id) 20)
          (take-refresh! wire method)
          (check-false (sync/timeout 0 wire))))
      ;; Non-matching capability suppresses refresh for this feature.
      (define other-capability (if (eq? capability 'semanticTokens) 'inlayHint 'semanticTokens))
      (define other-method (if (eq? other-capability 'semanticTokens)
                               semantic-tokens-refresh-method
                               inlay-hint-refresh-method))
      (with-server
        (capabilities (list other-capability))
        (lambda (server controller gates wire)
          (open-buffer! server)
          (define gate (await gates))
          (define fallback (capture-query server controller 21 feature))
          (accept! server controller gate wire)
          (take-refresh! wire other-method)
          (send server process-source-event fallback)
          (check-equal? (hash-ref (await wire) 'id) 21)
          (check-false (sync/timeout 0 wire)))))
    ;; Neither capability supported: no refresh on acceptance or fallback.
    (with-server
      (capabilities '())
      (lambda (server controller gates wire)
        (open-buffer! server)
        (define gate (await gates))
        (define fallback (capture-query server controller 22 'inlay-hints))
        (accept! server controller gate wire)
        (send server process-source-event fallback)
        (check-equal? (hash-ref (await wire) 'id) 22)
        (check-false (sync/timeout 0 wire)))))

  (test-case "reply validation, callback cleanup, and configuration payloads"
    (with-server
      (hasheq 'workspace (hasheq 'configuration #t))
      (lambda (server controller gates wire)
        (define seen '())
        (send server send-request "probe" (hasheq)
              (lambda (result) (set! seen (cons result seen))))
        (define request (await wire))
        (define id (hash-ref request 'id))
        (send server process-message (hasheq 'jsonrpc "2.0" 'id id))
        (define invalid (await wire))
        (check-equal? (hash-ref invalid 'id) id)
        (check-equal? (hash-ref (hash-ref invalid 'error) 'code) -32600)
        (check-equal? seen '())
        (send server process-message
              (hasheq 'jsonrpc "2.0" 'id id 'result #f))
        (reply! server request)
        (check-equal? seen (list #f))
        ;; Even a false error is present, and wins over a result.
        (send server send-request "both" (hasheq)
              (lambda (_) (error 'callback "error must win over result")))
        (define both (await wire))
        (send server process-message
              (hasheq 'jsonrpc "2.0" 'id (hash-ref both 'id) 'error #f 'result 1))
        (reply! server both) ; the error already removed its handler
        (send server send-request "throws" (hasheq) (lambda (_) (error 'callback "failed")))
        (define throwing (await wire))
        (check-exn exn:fail? (lambda () (reply! server throwing)))
        (reply! server throwing) ; removed before the throwing callback ran
        (send server send-request "reject" (hasheq)
              (lambda (_) (error 'callback "error reply must not invoke success callback")))
        (define rejection (hasheq 'code -32601 'message "no" 'data (hasheq 'reason 1)))
        (define rejected (await wire))
        (define logs (make-log-receiver racket-langserver-logger 'info))
        (reply! server rejected rejection)
        (match-define (vector _ rejected-msg _ _) (await logs))
        (check-regexp-match #rx"Client rejected server request" rejected-msg)
        (reply! server rejected rejection)
        (match-define (vector _ unknown-msg _ _) (await logs))
        (check-regexp-match #rx"Unknown client reply id" unknown-msg)
        ;; Exercise the existing configuration callback through real reply dispatch.
        (define previous (get-resyntax-enabled))
        (dynamic-wind
          void
          (lambda ()
            (send server process-message
                  (hasheq 'method "workspace/didChangeConfiguration"
                          'params (hasheq 'settings (hasheq))))
            (define config (await wire))
            (check-equal? (hash-ref config 'jsonrpc) "2.0")
            (check-equal? (hash-ref config 'method) "workspace/configuration")
            (send server process-message
                  (hasheq 'jsonrpc "2.0" 'id (hash-ref config 'id)
                          'result (list (hasheq 'resyntax (hasheq 'enable (not previous))))))
            (check-equal? (get-resyntax-enabled) (not previous)))
          (lambda () (set-resyntax-enabled! previous)))
        (check-false (sync/timeout 0 wire))))))
