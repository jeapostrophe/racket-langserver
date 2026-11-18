#lang racket/base

;; In-process LSP test sessions running the production server consumer.
;; `with-session` accepts an injected controller; `with-racket-lsp` handles
;; initialize/shutdown/exit around a default controller.
(provide (struct-out Session)
         with-session
         with-racket-lsp
         await
         response
         shutdown!
         notify
         request!
         open!
         close!
         change!
         client-send
         client-wait-response
         client-wait-notification
         make-request
         make-expected-response)

(require "silent-logger.rkt"
         json
         racket/async-channel
         racket/class
         racket/contract
         racket/match
         racket/os
         racket/path
         racket/runtime-path
         "../common/path-util.rkt"
         "../common/time.rkt"
         "../lsp/analysis-query.rkt"
         "../lsp/compose/definition.rkt"
         "../lsp/compose/hover.rkt"
         "../lsp/methods.rkt"
         "../lsp/source-controller.rkt"
         "../main.rkt")

(define-runtime-path root-folder "./")

(struct/contract Session
  ([controller Source-Controller?]
   [clients async-channel?]
   [responses async-channel?]
   [notifications async-channel?]
   [pending (hash/c (or/c exact-integer? string?)
                    (hash/c symbol? jsexpr? #:immutable #t)
                    #:immutable #f)]
   [next-id (-> exact-positive-integer?)])
  #:transparent)

(define (make-id-generator)
  (define id 0)
  (lambda ()
    (set! id (add1 id))
    id))

(define (await event [timeout 5])
  (define value (sync/timeout timeout (wrap-evt event list)))
  (unless value
    (error 'await "event did not complete within ~a seconds" timeout))
  (car value))

(define (response session id [timeout 5])
  (receive-response (Session-responses session) (Session-pending session) id timeout))

(define (receive-response responses pending id [timeout 5])
  (define deadline (deadline-after timeout))
  (define result
    (or (hash-ref pending id #f)
        (let loop ()
          (define remaining
            (max 0 (/ (- deadline (current-deadline-milliseconds)) 1000)))
          (define msg (sync/timeout remaining responses))
          (unless msg
            (error 'response "no response for id ~a within ~a seconds" id timeout))
          (define received-id (hash-ref msg 'id #f))
          (cond
            [(equal? received-id id) msg]
            [else
             (hash-set! pending received-id msg)
             (loop)]))))
  (hash-remove! pending id)
  result)

(define (with-session controller proc
                      #:request-timeout [timeout (current-analysis-request-timeout)]
                      #:hover-timeout [hover-timeout (current-hover-target-timeout)]
                      #:definition-timeout [definition-timeout (current-definition-target-timeout)])
  (define custodian (make-custodian))
  (define clients (make-async-channel))
  (define responses (make-async-channel))
  (define requests (make-async-channel))
  (define notifications (make-async-channel))
  (define session
    (Session controller clients responses notifications (make-hash) (make-id-generator)))
  (dynamic-wind
    void
    (lambda ()
      (parameterize ([current-custodian custodian]
                     [current-analysis-request-timeout timeout]
                     [current-hover-target-timeout hover-timeout]
                     [current-definition-target-timeout definition-timeout])
        (define server
          (new server%
            [source-controller controller]
            [response-channel responses]
            [request-channel requests]
            [notification-channel notifications]))
        (start-consumer server clients)
        (thread
          (lambda ()
            (let loop ()
              (match (async-channel-get requests)
                [(hash-table ['id id] ['method method])
                 (client-send session
                              (hasheq 'jsonrpc "2.0"
                                      'id id
                                      'result (if (equal? method "workspace/configuration")
                                                  (list (json-null))
                                                  (json-null))))]
                [_ (error 'with-session "not a request from server")])
              (loop))))
        (proc session)
        (void)))
    (lambda ()
      (dynamic-wind
        void
        (lambda ()
          (unless (sync/timeout 0 (source-controller-shutdown-evt controller))
            (shutdown! session)))
        (lambda () (custodian-shutdown-all custodian))))))

(define/contract (with-racket-lsp proc #:capabilities [capabilities (hasheq)])
  (->* ((-> Session? any/c)) (#:capabilities jsexpr?) void?)
  (define exited (make-async-channel))
  ;; Tests share a process: capture the production exit instead of terminating it.
  (parameterize ([exit-handler (lambda (code) (async-channel-put exited code))])
    (with-session
      (make-source-controller)
      (lambda (session)
        (define normalized-root (path->string (normalize-path root-folder)))
        (define init-req
          (make-request session "initialize"
                        (hasheq 'processId (getpid)
                                'capabilities capabilities
                                'rootPath normalized-root
                                'rootUri (string-append "file://" normalized-root))))
        (client-send session init-req)
        (client-wait-response session init-req)
        (proc session)
        (shutdown! session)
        (notify session "exit" #f)
        (define exit-code (await exited))
        (unless (eqv? exit-code 0)
          (error 'with-racket-lsp "server exited with ~v after shutdown" exit-code))))))

(define (shutdown! session)
  (async-channel-put (Session-clients session) (hasheq 'id "shutdown" 'method "shutdown"))
  (response session "shutdown"))

(define (notify session method params)
  (async-channel-put (Session-clients session) (hasheq 'method method 'params params)))

(define (document-uri target)
  (if (path? target) (path->uri target) target))

(define (open! session target text [version 0])
  (notify session "textDocument/didOpen"
          (hasheq 'textDocument (hasheq 'uri (document-uri target)
                                        'languageId "racket"
                                        'version version
                                        'text text))))

(define (close! session target)
  (notify session "textDocument/didClose"
          (hasheq 'textDocument (hasheq 'uri (document-uri target)))))

(define (change! session target text [version 1])
  (notify session "textDocument/didChange"
          (hasheq 'textDocument (hasheq 'uri (document-uri target) 'version version)
                  'contentChanges (list (hasheq 'text text)))))

(define (request! session id method target [extra (hasheq)])
  (async-channel-put
    (Session-clients session)
    (hasheq 'id id
            'method method
            'params (hash-set extra 'textDocument (hasheq 'uri (document-uri target))))))

(define/contract (client-send session req)
  (-> Session? jsexpr? void?)
  (async-channel-put (Session-clients session) req))

(define/contract (client-wait-response session req [timeout 90])
  (->* (Session? jsexpr?) (real?) jsexpr?)
  (response session (hash-ref req 'id) timeout))

(define (client-wait-notification session)
  (await (Session-notifications session) 90))

(define/contract (make-request session method params)
  (-> Session? string? jsexpr? jsexpr?)
  (define req
    (hasheq 'jsonrpc "2.0"
            'id ((Session-next-id session))
            'method method))
  (if params (hash-set req 'params params) req))

(define/contract (make-expected-response request result)
  (-> jsexpr? jsexpr? jsexpr?)
  (define res
    (hasheq 'jsonrpc "2.0"
            'id (hash-ref request 'id)))
  (if result (hash-set res 'result result) res))

(module+ test
  (require rackunit)

  (test-case "response waits preserve replies that arrive in reverse request order"
    (define responses (make-async-channel))
    (define pending (make-hash))
    (define first (hasheq 'id 1 'result '()))
    (define second (hasheq 'id 2 'error (hasheq 'code -32803 'message "failed")))
    (async-channel-put responses second)
    (async-channel-put responses first)
    (check-equal? (receive-response responses pending 1) first)
    (check-equal? (receive-response responses pending 2) second)
    (check-true (hash-empty? pending))))
