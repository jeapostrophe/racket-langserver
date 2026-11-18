#lang racket/base

;; Shared controller observations, queue barriers, gates, and temp-directory
;; fixtures for `tests/lsp/` tests.
(require "../silent-logger.rkt"
         rackunit
         racket/async-channel
         racket/file
         racket/string
         "../client.rkt"
         "../../common/interfaces.rkt"
         "../../common/json-util.rkt"
         "../../common/path-util.rkt"
         "../../common/settings.rkt"
         "../../common/source.rkt"
         "../../lsp/analysis.rkt"
         "../../lsp/safedoc.rkt"
         "../../lsp/source-controller.rkt"
         (only-in (submod "../../lsp/source-controller.rkt" owner)
                  source-controller-handle!
                  source-controller-open!
                  source-controller-shutdown!)
         "../../workspace/source-store.rkt")

(provide await
         observation
         acquire
         view
         refresh
         notify
         open!
         close!
         change!
         request!
         check-error
         barrier
         shutdown!
         response
         with-session
         drive-controller
         scratch
         write-text
         with-temp-dir
         notifications
         with-buffer
         start-deferred
         (struct-out Session)
         (struct-out Gate)
         wait-at-gate
         release-gate!
         controlled-analyzer
         plain-controller
         gated-analyzer
         gated-reader
         with-example
         check-location
         call-with-resyntax-enabled
         saved
         unsaved
         a-text)

(define scratch (find-system-path 'temp-dir))

(define saved "#lang racket/base\n(provide shared)\n(define shared 1)\n")
(define unsaved "#lang racket/base\n(provide shared)\n\n;; Unsaved\n(define shared 2)\n")
(define a-text "#lang racket/base\n(require \"b.rkt\")\n(define local 1)\n(list shared local)\n")

(define (write-text path text)
  (display-to-file text path #:exists 'truncate))

(define (with-temp-dir prefix proc)
  (define root (make-temporary-file (string-append prefix "~a") 'directory))
  (dynamic-wind
    void
    (lambda () (proc root))
    (lambda () (delete-directory/files root))))

(define (observation session target)
  (define result (await (refresh session target)))
  (when (Source-Failure? result)
    (error 'observation "source command failed: ~v" (Source-Failure-reason result)))
  result)

;; Observation of a closed file reads disk. Tests with a held read must use
;; their captured result/view and current-view validation instead.
(define (view session target)
  (Source-Result-view (observation session target)))

(define (acquire session target)
  (define result (observation session target))
  (when (Source-Result-error result)
    (error 'acquire "source observation failed: ~v" (Source-Result-error result)))
  (define acquisition
    (await (source-controller-analysis-evt (Session-controller session)
                                           (Source-Result-snapshot result))))
  (when (Source-Failure? acquisition)
    (error 'acquire "analysis command failed: ~v" (Source-Failure-reason acquisition)))
  (Analysis-Handle-completion-evt acquisition))

(define (refresh session path)
  (source-controller-observe-evt (Session-controller session)
                                 (if (Source? path) path (path->source path))))

;; A client request is an ordering barrier for preceding client notifications.
;; Controller commands are a separate queue and cannot serve that purpose.
(define (barrier session)
  (async-channel-put (Session-clients session)
                     (hasheq 'id 100 'method "textDocument/hover" 'params (hasheq)))
  (response session 100))

(define (check-error result code [message #f])
  (define err (hash-ref result 'error))
  (check-equal? (hash-ref err 'code) (->jsexpr code))
  (when message
    (check-equal? (hash-ref err 'message) message)))

;; Drive only a test-owned controller. Consumer sessions have their own event pump.
(define (drive-controller controller event [timeout 5])
  (let loop ()
    (define next
      (await (choice-evt (wrap-evt event (lambda (value) (cons 'result value)))
                         (wrap-evt (source-controller-events controller)
                                   (lambda (value) (cons 'event value))))
             timeout))
    (if (eq? (car next) 'result)
        (cdr next)
        (begin
          (source-controller-handle! controller (cdr next))
          (loop)))))

(define (notifications session)
  (let loop ([messages '()])
    (define msg (sync/timeout 0 (Session-notifications session)))
    (if msg
        (loop (cons msg messages))
        (reverse messages))))

(define (with-buffer controller path text proc)
  (define custodian (make-custodian))
  (parameterize ([current-custodian custodian]
                 [current-source-controller controller])
    (define source (path->source path))
    (define sd (new-safedoc (path->uri path) text 7))
    (dynamic-wind
      (lambda () (source-controller-open! controller source sd))
      (lambda () (proc sd source))
      (lambda ()
        (source-controller-shutdown! controller)
        (custodian-shutdown-all custodian)))))

(define (start-deferred deferred)
  (define responses (make-async-channel))
  (thread (lambda () (async-channel-put responses (deferred))))
  responses)

;; Receiving a Gate establishes that this invocation reached the held work.
(struct Gate (input release) #:transparent)

(define (wait-at-gate gates input)
  (define release (make-async-channel))
  (async-channel-put gates (Gate input release))
  (async-channel-get release))

(define (release-gate! gate [command 'go])
  (async-channel-put (Gate-release gate) command))

(define (controlled-analyzer gates [target-path #f])
  (lambda (snapshot)
    (when (or (not target-path)
              (equal? (source-id (path->source target-path)) (Source-Snapshot-id snapshot)))
      (case (wait-at-gate gates snapshot)
        [(raise) (raise (if target-path 'target-failed 'arbitrary-analysis-failure))]
        [(die)
         ;; Kill from a separate thread so parallel self-kill scheduling
         ;; cannot turn worker-exited into a timeout.
         (define worker (current-thread))
         (thread (lambda () (kill-thread worker)))
         (sync never-evt)]))
    (analyze-source snapshot)))

(define (plain-controller _b)
  (make-source-controller #:resyntax #f))

(define (gated-analyzer gates)
  (lambda (b)
    (make-source-controller #:analyze (controlled-analyzer gates b) #:resyntax #f)))

(define (gated-reader reads)
  (lambda (_b)
    (make-source-controller
      #:read (lambda (path)
               (case (wait-at-gate reads path)
                 [(raise) (raise 'read-failed)]
                 [(missing) (Disk-Missing)]
                 [else (file->string path)]))
      #:resyntax #f)))

(define (with-example make-controller proc [source a-text]
                      #:relative? [relative? #f]
                      #:target-text [target-text saved]
                      #:request-timeout [request-timeout 90]
                      #:hover-timeout [hover-timeout 90])
  (with-temp-dir
    "definition-controller"
    (lambda (root)
      (define a (build-path root "a.rkt"))
      (define b (build-path root "b.rkt"))
      (define direct-source
        (if relative?
            source
            (string-replace source "\"b.rkt\"" (format "(file ~s)" (path->string b)))))
      (write-text a direct-source)
      (write-text b target-text)
      (with-session
        (make-controller b)
        (lambda (session)
          (open! session a direct-source)
          (barrier session)
          (check-eq? (Analysis-Outcome-state (await (acquire session a) 90)) 'succeeded)
          (proc session a b root)
          (close! session a)
          (barrier session))
        #:definition-timeout request-timeout
        #:hover-timeout hover-timeout))))

;; Columns 8 through 14 span the shared identifier in the target fixtures.
(define (check-location session id b [line 2])
  (check-equal? (hash-ref (response session id) 'result)
                (->jsexpr (Location (path->uri b) (Range (Pos line 8) (Pos line 14))))))

(define (call-with-resyntax-enabled enabled? proc)
  (define previous (get-resyntax-enabled))
  (dynamic-wind
    (lambda () (set-resyntax-enabled! enabled?))
    proc
    (lambda () (set-resyntax-enabled! previous))))
