#lang racket

;; In-process test sessions run the production consumer. The callback receives a Session.
;; with-racket-lsp adds initialization/exit; with-session accepts an injected controller.
(provide with-racket-lsp with-session (struct-out Session)
         client-send client-wait-response client-wait-notification client-has-diagnostic?
         make-request make-expected-response make-notification
         await response shutdown! notify request! open! close! change!)

(require racket/os
         racket/async-channel
         racket/match
         racket/runtime-path
         json
         "../common/json-util.rkt"
         "../common/path-util.rkt"
         "../common/time.rkt"
         "../lsp/methods.rkt"
         "../lsp/analysis-query.rkt"
         "../lsp/source-controller.rkt"
         "../main.rkt")

(define id 0)
(define (genid)
  (set! id (add1 id))
  id)

(define current-session (make-parameter #f))
(define-runtime-path rootFolder "./")

(struct/contract Session
  ([controller Source-Controller?]
   [clients async-channel?]
   [responses async-channel?]
   [notifications async-channel?]
   [pending (hash/c (or/c exact-integer? string?)
                    (hash/c symbol? jsexpr? #:immutable #t)
                    #:immutable #f)])
  #:transparent)

(define (await event [timeout 5])
  (define value (sync/timeout timeout (wrap-evt event list)))
  (unless value (error 'await "event did not complete within ~a seconds" timeout))
  (car value))

(define (response session id [timeout 5])
  (receive-response (Session-responses session) (Session-pending session) id timeout))

(define (receive-response responses pending id [timeout 5])
  (define deadline (+ (current-deadline-milliseconds) (* timeout 1000)))
  (define result
    (or (hash-ref pending id #f)
        (let loop ()
          (define msg
            (sync/timeout (max 0 (/ (- deadline (current-deadline-milliseconds)) 1000))
                          responses))
          (unless msg (error 'response "no response for id ~a within ~a seconds" id timeout))
          (define received-id (hash-ref msg 'id #f))
          (cond
            [(equal? received-id id) msg]
            [else (hash-set! pending received-id msg) (loop)]))))
  (hash-remove! pending id)
  result)

;; Both initialized feature tests and injected-controller tests run the production consumer.
(define (with-session controller proc
                      #:request-timeout [timeout (current-analysis-request-timeout)])
  (define custodian (make-custodian))
  (define clients (make-async-channel))
  (define responses (make-async-channel))
  (define requests (make-async-channel))
  (define notifications (make-async-channel))
  (define session (Session controller clients responses notifications (make-hash)))
  (dynamic-wind
    void
    (lambda ()
      (parameterize ([current-custodian custodian]
                     [current-analysis-request-timeout timeout]
                     [current-session session])
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
                              (hasheq 'jsonrpc "2.0" 'id id
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
  (->* ((-> any/c any/c)) (#:capabilities jsexpr?) void?)
  (define exited (make-async-channel))
  (parameterize ([exit-handler (lambda (code) (async-channel-put exited code))])
    (with-session
      (make-source-controller)
      (lambda (session)
        (define init-req
          (make-request session "initialize"
                        (hasheq 'processId (getpid)
                                'capabilities capabilities
                                'rootPath (path->string (normalize-path rootFolder))
                                'rootUri (string-append "file://" (path->string (normalize-path rootFolder))))))
        (client-send session init-req)
        (client-wait-response init-req)
        (proc session)
        (define shutdown-req (make-request session "shutdown" #f))
        (client-send session shutdown-req)
        (client-wait-response shutdown-req)
        ;; Tests share a process: capture the production exit instead of terminating it.
        (client-send session (make-notification "exit" #f))
        (define exit-code (await exited))
        (unless (eqv? exit-code 0)
          (error 'with-racket-lsp "server exited with ~v after shutdown" exit-code))))))

(define (shutdown! session)
  (async-channel-put (Session-clients session) (hasheq 'id "shutdown" 'method "shutdown"))
  (response session "shutdown"))

(define (notify session method params)
  (async-channel-put (Session-clients session) (hasheq 'method method 'params params)))

(define (open! session target text [version 0])
  (notify session "textDocument/didOpen"
          (hasheq 'textDocument (hasheq 'uri (document-uri target) 'languageId "racket"
                                        'version version 'text text))))

(define (close! session target)
  (notify session "textDocument/didClose"
          (hasheq 'textDocument (hasheq 'uri (document-uri target)))))

;; A target is a file path or an original client URI; preserve URI spelling.
(define (document-uri target)
  (if (path? target) (path->uri target) target))

(define (request! session id method target [extra (hasheq)])
  (async-channel-put
    (Session-clients session)
    (hasheq 'id id 'method method
            'params (hash-set extra 'textDocument (hasheq 'uri (document-uri target))))))

(define (change! session target text [version 1])
  (notify session "textDocument/didChange"
          (hasheq 'textDocument (hasheq 'uri (document-uri target) 'version version)
                  'contentChanges (list (hasheq 'text text)))))

(define/contract (client-send session req)
  (-> any/c jsexpr? void?)
  (async-channel-put (Session-clients session) req))

;; Existing fixture tests pass request objects; controller tests use explicit IDs.
(define/contract (client-wait-response req)
  (-> any/c jsexpr?)
  (response (current-session) (hash-ref req 'id) 90))

(define (client-wait-notification session)
  (await (Session-notifications session) 90))

(define/contract (client-has-diagnostic? notification expected-diagnostic)
  (-> jsexpr? hash? boolean?)

  (for/or ([diagnostic (in-list (jsexpr-ref notification '(params diagnostics)))])
    (equal? diagnostic expected-diagnostic)))

(define/contract (make-request lsp method params)
  (-> any/c string? jsexpr? jsexpr?)

  (define req
    (hasheq 'jsonrpc "2.0"
            'id (genid)
            'method method))
  (cond [(not params) req]
        [else (hash-set req 'params params)]))

(define/contract (make-expected-response request result)
  (-> jsexpr? jsexpr? jsexpr?)

  (define res
    (hasheq 'jsonrpc "2.0"
            'id (hash-ref request 'id)))
  (cond [(not result) res]
        [else (hash-set res 'result result)]))

(define/contract (make-notification method params)
  (-> string? jsexpr? jsexpr?)

  (define req
    (hasheq 'jsonrpc "2.0"
            'method method))
  (cond [(not params) req]
        [else (hash-set req 'params params)]))


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
