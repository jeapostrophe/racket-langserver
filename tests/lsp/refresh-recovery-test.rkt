#lang racket/base

;; Drive the real protocol dispatcher with held analysis and one outbound queue.
;; Explicitly dispatching consumer events makes response/refresh order observable.
(module+ test
  (require "controller-support.rkt"
           "../json-dsl.rkt"
           rackunit
           racket/async-channel
           racket/class
           "../../common/debug.rkt"
           "../../common/interfaces.rkt"
           "../../common/json-util.rkt"
           "../../common/settings.rkt"
           "../../lsp/analysis-query.rkt"
           "../../lsp/methods.rkt"
           "../../lsp/source-controller.rkt"
           (submod "../../lsp/source-controller.rkt" owner))

  (define uri "untitled:refresh-recovery")
  (define text "#lang racket/base\n(struct point (x y))\n(point 1 2)\n")
  (define semantic-tokens-refresh-method "workspace/semanticTokens/refresh")
  (define inlay-hint-refresh-method "workspace/inlayHint/refresh")
  (define refresh-methods (list semantic-tokens-refresh-method inlay-hint-refresh-method))

  (define (with-server capabilities proc)
    (define gates (make-async-channel))
    (define wire (make-async-channel))
    (define controller
      (make-source-controller #:resyntax #f #:analyze (controlled-analyzer gates)))
    (define server
      (new server%
        [source-controller controller]
        [response-channel wire]
        [request-channel wire]
        [notification-channel wire]))
    (dynamic-wind
      void
      (lambda ()
        (send server process-message
              (json {#:id "init"
                     #:method "initialize"
                     #:params {#:processId null
                               #:rootUri null
                               #:rootPath null
                               #:capabilities capabilities}}))
        (check-equal? (hash-ref (await wire) 'id) "init")
        (proc server controller gates wire))
      (lambda () (source-controller-shutdown! controller))))

  (define (capabilities features)
    (json {#:workspace
           (for/hasheq ([feature (in-list features)])
             (values feature (json {#:refreshSupport #t})))}))

  (define (open-buffer! server)
    (send server process-message
          (json {#:method "textDocument/didOpen"
                 #:params {#:textDocument (->jsexpr (DocItem uri "racket" 0 text))}})))

  (define (close-buffer! server)
    (send server process-message
          (json {#:method "textDocument/didClose"
                 #:params {#:textDocument {#:uri uri}}})))

  (define (capture-query server controller id feature)
    (parameterize ([current-analysis-request-timeout 0])
      (send server process-message
            (json {#:id id
                   #:method (if (eq? feature 'semantic-tokens)
                                "textDocument/semanticTokens/full"
                                "textDocument/inlayHint")
                   #:params {#:textDocument {#:uri uri}
                             #:range (->jsexpr (Range (Pos 0 0) (Pos 50 0)))}})))
    (let loop ()
      (define event (await (source-controller-events controller)))
      (cond
        [(Visual-Delivery? event) event]
        [else
         (send server process-source-event event)
         (loop)])))

  (define (accept! server controller gate wire)
    (release-gate! gate)
    (send server process-source-event (await (source-controller-events controller) 90))
    (check-equal? (hash-ref (await wire) 'method) "textDocument/publishDiagnostics"))

  (define (take-refresh! wire method)
    (define request (await wire))
    (check-equal? (hash-ref request 'jsonrpc) "2.0")
    (check-equal? (hash-ref request 'method) method)
    (check-equal? (hash-ref request 'params) (json {}))
    request)

  (define (reply! server request [error #f])
    (send server process-message
          (hash-set (json {#:jsonrpc "2.0" #:id (hash-ref request 'id)})
                    (if error 'error 'result)
                    (or error (json null)))))

  (define (edit! server version [changes (list (->jsexpr (ContentChangeEvent-Full text)))])
    (send server process-message
          (json {#:method "textDocument/didChange"
                 #:params {#:textDocument (->jsexpr (DocIdentifier version uri))
                           #:contentChanges changes}})))

  (test-case "degraded visuals recover on acceptance; error acceptance sends no refresh"
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
        (for ([method (in-list refresh-methods)])
          (take-refresh! wire method))
        (define fresh (capture-query server controller 2 'inlay-hints))
        (check-false (Visual-Delivery-degraded? fresh))
        (send server process-source-event fresh)
        (check-not-equal? (hash-ref (await wire) 'result) '())
        (check-false (sync/timeout 0 wire))
        (edit! server 1
               (list (hasheq 'range (hasheq 'start (hasheq 'line 3 'character 0)
                                            'end (hasheq 'line 3 'character 0))
                             'text "(define")))
        (accept! server controller (await gates) wire)
        (check-false (sync/timeout 0 wire)))))

  (test-case "overlapping refresh requests stay independent across out-of-order replies"
    (with-server
      (capabilities '(semanticTokens inlayHint))
      (lambda (server controller gates wire)
        (open-buffer! server)
        (accept! server controller (await gates) wire)
        (define tokens (take-refresh! wire semantic-tokens-refresh-method))
        (define hints (take-refresh! wire inlay-hint-refresh-method))
        (edit! server 1)
        (accept! server controller (await gates) wire)
        (define newer-tokens (take-refresh! wire semantic-tokens-refresh-method))
        (define newer-hints (take-refresh! wire inlay-hint-refresh-method))
        (check-not-equal? (hash-ref tokens 'id) (hash-ref newer-tokens 'id))
        (reply! server newer-hints)
        (reply! server tokens (hasheq 'code -32601 'message "unsupported"))
        (reply! server hints)
        (reply! server newer-tokens)
        (check-false (sync/timeout 0 wire)))))

  (test-case "success before delayed delivery queues fallback then its feature refresh"
    (with-server
      (capabilities '(semanticTokens inlayHint))
      (lambda (server controller gates wire)
        (open-buffer! server)
        (define first (await gates))
        (define tokens (capture-query server controller 10 'semantic-tokens))
        (define hints (capture-query server controller 11 'inlay-hints))
        (accept! server controller first wire)
        (for ([method (in-list refresh-methods)])
          (take-refresh! wire method))
        (for ([delivery (in-list (list tokens hints))]
              [method (in-list refresh-methods)])
          (send server process-source-event delivery)
          (check-equal? (await wire) (Visual-Delivery-response delivery))
          (take-refresh! wire method)
          (check-false (sync/timeout 0 wire))))))

  (test-case "stale or shutdown visual deliveries cancel without sending refresh"
    (with-server
      (capabilities '(semanticTokens inlayHint))
      (lambda (server controller gates wire)
        (open-buffer! server)
        (accept! server controller (await gates) wire)
        (for ([method (in-list refresh-methods)])
          (take-refresh! wire method))
        (for ([op (in-list '(edit close))]
              [id (in-list '(12 13))])
          (define stale (capture-query server controller id 'semantic-tokens))
          (if (eq? op 'edit)
              (edit! server 1)
              (begin
                (close-buffer! server)
                (check-equal? (hash-ref (await wire) 'method) "textDocument/publishDiagnostics")))
          (send server process-source-event stale)
          (check-error (await wire) ErrorCode-ContentModified)
          (check-false (sync/timeout 0 wire))
          (when (eq? op 'edit) (await gates)))
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
    (define (check-refresh-gating! caps feature id [accept-refreshes '()] [fallback-refreshes '()])
      (with-server caps
                   (lambda (server controller gates wire)
                     (open-buffer! server)
                     (define gate (await gates))
                     (define fallback (capture-query server controller id feature))
                     (accept! server controller gate wire)
                     (for ([method (in-list accept-refreshes)])
                       (take-refresh! wire method))
                     (send server process-source-event fallback)
                     (check-equal? (hash-ref (await wire) 'id) id)
                     (for ([method (in-list fallback-refreshes)])
                       (take-refresh! wire method))
                     (check-false (sync/timeout 0 wire)))))

    (for ([row (in-list
                 (list (list (capabilities '(semanticTokens)) 'semantic-tokens 20
                             (list semantic-tokens-refresh-method)
                             (list semantic-tokens-refresh-method))
                       (list (capabilities '(inlayHint)) 'semantic-tokens 21
                             (list inlay-hint-refresh-method) '())
                       (list (capabilities '(inlayHint)) 'inlay-hints 22
                             (list inlay-hint-refresh-method) (list inlay-hint-refresh-method))
                       (list (capabilities '(semanticTokens)) 'inlay-hints 23
                             (list semantic-tokens-refresh-method) '())
                       (list (capabilities '()) 'inlay-hints 24 '() '())
                       (list (json {#:workspace {#:semanticTokens #t #:inlayHint #t}})
                             'inlay-hints 25 '() '())))])
      (apply check-refresh-gating! row)))

  (test-case "client reply envelopes validate fields, prioritize errors, and remove handlers once"
    (with-server
      (json {})
      (lambda (server _controller _gates wire)
        (define seen '())
        (send server send-request "probe" (json {})
              (lambda (result) (set! seen (cons result seen))))
        (define request (await wire))
        (define id (hash-ref request 'id))
        (send server process-message (json {#:jsonrpc "2.0" #:id id}))
        (define invalid (await wire))
        (check-equal? (hash-ref invalid 'id) id)
        (check-equal? (hash-ref (hash-ref invalid 'error) 'code)
                      (->jsexpr ErrorCode-InvalidRequest))
        (check-equal? seen '())
        (send server process-message (json {#:jsonrpc "2.0" #:id id #:result #f}))
        (check-equal? seen (list #f))
        (send server send-request "both" (json {})
              (lambda (_) (error 'callback "error must win over result")))
        (define both (await wire))
        (send server process-message
              (json {#:jsonrpc "2.0" #:id (hash-ref both 'id) #:error #f #:result 1}))
        (send server send-request "throws" (json {}) (lambda (_) (error 'callback "failed")))
        (define throwing (await wire))
        (check-exn exn:fail? (lambda () (reply! server throwing)))
        (define logs (make-log-receiver racket-langserver-logger 'info))
        (reply! server throwing)
        (check-regexp-match #rx"Unknown client reply id" (vector-ref (await logs) 1))
        (send server send-request "reject" (json {})
              (lambda (_) (error 'callback "error reply must not invoke success callback")))
        (define rejected (await wire))
        (reply! server rejected
                (json {#:code (->jsexpr ErrorCode-MethodNotFound)
                       #:message "no"
                       #:data {#:reason 1}}))
        (check-regexp-match #rx"Client rejected server request" (vector-ref (await logs) 1))
        (reply! server rejected)
        (check-regexp-match #rx"Unknown client reply id" (vector-ref (await logs) 1))
        (check-false (sync/timeout 0 wire)))))

  (test-case "workspace/didChangeConfiguration round-trip updates resyntax setting"
    (with-server
      (json {#:workspace {#:configuration #t}})
      (lambda (server _controller _gates wire)
        (define previous (get-resyntax-enabled))
        (call-with-resyntax-enabled
          previous
          (lambda ()
            (send server process-message
                  (json {#:method "workspace/didChangeConfiguration"
                         #:params {#:settings {}}}))
            (define config (await wire))
            (check-equal? (hash-ref config 'jsonrpc) "2.0")
            (check-equal? (hash-ref config 'method) "workspace/configuration")
            (send server process-message
                  (json {#:jsonrpc "2.0"
                         #:id (hash-ref config 'id)
                         #:result [{#:resyntax {#:enable (not previous)}}]}))
            (check-equal? (get-resyntax-enabled) (not previous))))
        (check-false (sync/timeout 0 wire))))))
