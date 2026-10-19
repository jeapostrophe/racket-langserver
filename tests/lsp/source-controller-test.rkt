#lang racket/base

;; Source-controller checks through the production consumer. The test holds
;; disk reads, so ordering does not depend on sleeps. A direct process-message
;; call would skip the client and internal event pump.
(require "../../common/source.rkt"
         json
         rackunit
         racket/async-channel
         racket/class
         racket/contract
         racket/file
         racket/match
         racket/path
         racket/runtime-path
         "../../main.rkt"
         "../../common/path-util.rkt"
         "../../common/time.rkt"
         "../../lsp/methods.rkt"
         "../../lsp/analysis.rkt"
         "../../lsp/analysis-query.rkt"
         "../../lsp/source-controller.rkt"
         (only-in "../../lsp/source-controller/disk-reader.rkt" read-source-text)
         "../../workspace/source-store.rkt")

(define-runtime-path scratch-path "../../.agent-scratch")

(define scratch (simplify-path scratch-path #f))

(struct/contract Probe ([path path?] [release async-channel?]) #:transparent)

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
  (Analysis-Acquisition-completion acquisition))

;; Validation orders preceding controller commands without reading disk or
;; starting analysis. A different store's revision can never be current in this session.
(define barrier-view
  (source-store-view (make-source-store) (path->source (build-path scratch "barrier.rkt"))))

(define (controller-barrier session)
  (check-false
    (await (source-controller-current-view-evt (Session-controller session) barrier-view))))

(define (shutdown! session)
  (async-channel-put (Session-clients session) (hasheq 'id "shutdown" 'method "shutdown"))
  (response session "shutdown"))

(define (refresh session path)
  (source-controller-observe-evt (Session-controller session)
                                 (if (Source? path) path (path->source path))))

(define (notify session method params)
  (async-channel-put (Session-clients session) (hasheq 'method method 'params params)))

(define (open! session path text [version 0])
  (notify session "textDocument/didOpen"
          (hasheq 'textDocument (hasheq 'uri (path->uri path) 'languageId "racket"
                                        'version version 'text text))))

(define (close! session path)
  (notify session "textDocument/didClose"
          (hasheq 'textDocument (hasheq 'uri (path->uri path)))))

;; A client request is an ordering barrier for preceding client notifications.
;; Controller commands are a separate queue and cannot serve that purpose.
(define (barrier session)
  (async-channel-put (Session-clients session)
                     (hasheq 'id 100 'method "textDocument/hover"
                             'params (hasheq)))
  (response session 100))

(define (response session id)
  (define pending (Session-pending session))
  (define deadline (+ (current-deadline-milliseconds) 5000))
  (define result
    (or (hash-ref pending id #f)
        (let loop ()
          (define msg
            (sync/timeout (max 0 (/ (- deadline (current-deadline-milliseconds)) 1000))
                          (Session-responses session)))
          (unless msg (error 'response "no response for id ~a within 5 seconds" id))
          (define received-id (hash-ref msg 'id #f))
          (cond
            [(equal? received-id id) msg]
            [else (hash-set! pending received-id msg) (loop)]))))
  (hash-remove! pending id)
  result)

(define (with-session controller proc
                      #:request-timeout [timeout (current-analysis-request-timeout)])
  (define custodian (make-custodian))
  (define clients (make-async-channel))
  (define responses (make-async-channel))
  (define notifications (make-async-channel))
  (define session (Session controller clients responses notifications (make-hash)))
  (parameterize ([current-custodian custodian]
                 [current-analysis-request-timeout timeout])
    (define server
      (new server%
        [source-controller controller]
        [response-channel responses]
        [request-channel responses]
        [notification-channel notifications]))
    (start-consumer server clients))
  (dynamic-wind
    void
    (lambda () (proc session) (void))
    (lambda ()
      (unless (sync/timeout 0 (source-controller-shutdown-evt controller))
        (shutdown! session))
      (custodian-shutdown-all custodian))))

(define (controlled-controller probes #:timeout [timeout 90])
  (make-source-controller
    #:read-timeout timeout
    #:read (lambda (path)
             (define release (make-async-channel))
             (async-channel-put probes (Probe path release))
             (match (async-channel-get release)
               ['raise (raise 'arbitrary-read-failure)]
               ['die (kill-thread (current-thread))]
               [value value]))))

(define (release! probe value)
  (async-channel-put (Probe-release probe) value))

(define (result-text result)
  (Source-Snapshot-text (Source-Result-snapshot result)))

(define (with-files proc)
  (make-directory* scratch)
  (define root (make-temporary-file "source-controller~a" 'directory scratch))
  (dynamic-wind void (lambda () (proc root)) (lambda () (delete-directory/files root))))

(module+ test-support
  (provide await observation acquire view refresh notify open! close! barrier controller-barrier
           shutdown! response with-session
           scratch (struct-out Session)))

(module+ test
  (test-case "controller contracts reject invalid readers, timeouts, and results"
    (check-exn exn:fail:contract? (lambda () (make-source-controller #:read #f)))
    (check-exn exn:fail:contract? (lambda () (make-source-controller #:read (lambda () "text"))))
    (check-exn exn:fail:contract? (lambda () (make-source-controller #:read-timeout -1)))
    (check-exn exn:fail:contract? (lambda () (Source-Result 'invalid-view #f #f)))
    (define view
      (source-store-view (make-source-store) (path->source (build-path scratch "contract.rkt"))))
    (check-exn exn:fail:contract? (lambda () (Source-Result view #f 'invalid-error))))

  (test-case "disk reader returns content and reports a missing file"
    (with-files
      (lambda (root)
        (define path (build-path root "reader.rkt"))
        (display-to-file "hello" path)
        (check-equal? "hello" (read-source-text path))
        (delete-file path)
        (check-true (Disk-Missing? (read-source-text path))))))

  (test-case "ordered refresh barriers cannot reuse a read started before the command"
    (define probes (make-async-channel))
    (with-session (controlled-controller probes)
                  (lambda (session)
                    (define path (build-path scratch "ordered.rkt"))
                    (define first (refresh session path))
                    (define read-1 (await probes))
                    (define second (refresh session path))
                    (define third (refresh session path))
                    (controller-barrier session) ; internal FIFO has accepted both pending refreshes
                    (release! read-1 "old")
                    (check-equal? "old" (result-text (await first)))
                    (define old-view (Source-Result-view (await first)))
                    (define read-2 (await probes))
                    (check-false (sync/timeout 0 second))
                    (release! read-2 "new")
                    (define result-2 (await second))
                    (define result-3 (await third))
                    (check-eq? result-2 (await second)
                               "a completed read is observable without consumption")
                    (check-eq? result-2 (await third) "peers share the exact write-once result")
                    (check-equal? "new" (result-text result-2))
                    (check-eq? (Source-Result-view result-2) (Source-Result-view result-3))
                    (check-not-false (sync/timeout 0 (Source-View-changed old-view)))
                    (check-false (sync/timeout 0 probes)))))

  (test-case "real consumer applies edits and answers local queries while disk is held"
    (define probes (make-async-channel))
    (with-session (controlled-controller probes)
                  (lambda (session)
                    (define a (build-path scratch "responsive-a.rkt"))
                    (define b (build-path scratch "responsive-b.rkt"))
                    (define b-result (refresh session b))
                    (define b-read (await probes))
                    (open! session a "#lang racket/base\n(define value 1)\nvalue\n")
                    (barrier session)
                    ;; Wait for publishDiagnostics, which Check Syntax sends
                    ;; before installing the trace. Then query the local binding.
                    (let loop ()
                      (define msg (await (Session-notifications session)))
                      (unless (equal? (hash-ref msg 'method #f) "textDocument/publishDiagnostics")
                        (loop)))
                    (define old-view (view session a))
                    (notify session "textDocument/didChange"
                            (hasheq 'textDocument (hasheq 'uri (path->uri a) 'version 1)
                                    'contentChanges
                                    (list (hasheq 'range
                                                  (hasheq 'start (hasheq 'line 3 'character 0)
                                                          'end (hasheq 'line 3 'character 0))
                                                  'text "\n"))))
                    (async-channel-put
                      (Session-clients session)
                      (hasheq 'id 101 'method "textDocument/definition"
                              'params (hasheq 'textDocument (hasheq 'uri (path->uri a))
                                              'position (hasheq 'line 2 'character 0))))
                    (define local (response session 101))
                    (check-equal? (hash-ref (hash-ref local 'result) 'uri) (path->uri a))
                    (check-not-false (sync/timeout 0 (Source-View-changed old-view)))
                    (define c-result (refresh session (build-path scratch "independent-c.rkt")))
                    (define c-read (await probes))
                    (release! c-read "independent")
                    (check-equal? "independent" (result-text (await c-result)))
                    (check-false (sync/timeout 0 b-result))
                    (release! b-read "held")
                    (check-equal? "held" (result-text (await b-result)))
                    (close! session a)
                    (barrier session))))

  (test-case "open/close/reopen invalidates a held disk read and reused client versions"
    (define probes (make-async-channel))
    (with-session (controlled-controller probes)
                  (lambda (session)
                    (define path (build-path scratch "authority.rkt"))
                    (define disk-result (refresh session path))
                    (define stale-read (await probes))
                    (open! session path "#lang racket/base\n; unsaved\n" 7)
                    (barrier session)
                    (check-equal? 'buffer
                                  (Source-View-origin (Source-Result-view (await disk-result))))
                    (define opened (view session path))
                    (check-equal? "#lang racket/base\n; unsaved\n"
                                  (result-text (await (refresh session path))))
                    (close! session path)
                    (barrier session)
                    (check-false
                      (await (source-controller-current-view-evt
                               (Session-controller session) opened)))
                    (check-not-false (sync/timeout 0 (Source-View-changed opened)))
                    (check-false (sync/timeout 0 probes) "close starts no read")
                    (define saved-result (refresh session path))
                    (define saved-read (await probes))
                    (open! session path "#lang racket/base\n; reopened\n" 7)
                    (barrier session)
                    (define reopened (view session path))
                    (check-not-eq? (Source-View-revision opened) (Source-View-revision reopened))
                    (check-equal? "#lang racket/base\n; reopened\n"
                                  (result-text (await saved-result)))
                    (release! stale-read "obsolete")
                    (release! saved-read "saved")
                    (check-equal? "#lang racket/base\n; reopened\n"
                                  (result-text (await (refresh session path))))
                    (close! session path)
                    (barrier session)
                    (define final (refresh session path))
                    (release! (await probes) "actually saved")
                    (check-equal? "actually saved" (result-text (await final))))))

  (test-case "disk freshness compares content even when size and mtime are restored"
    (with-files
      (lambda (root)
        (define path (build-path root "disk.rkt"))
        (display-to-file "one" path)
        (define mtime (file-or-directory-modify-seconds path))
        (with-session (make-source-controller)
                      (lambda (session)
                        (define first (await (refresh session path)))
                        (define identical (await (refresh session path)))
                        (check-eq? (Source-Result-view first) (Source-Result-view identical))
                        (display-to-file "two" path #:exists 'truncate)
                        (file-or-directory-modify-seconds path mtime)
                        (define changed (await (refresh session path)))
                        (check-equal? "two" (result-text changed))
                        (check-not-eq? (Source-Result-view first) (Source-Result-view changed))
                        (delete-file path)
                        (define missing (await (refresh session path)))
                        (check-eq? 'file-not-found (Source-View-content-state (Source-Result-view missing)))
                        (check-false (Source-Result-error missing))
                        (display-to-file "new" path)
                        (check-equal? "new" (result-text (await (refresh session path)))))))))

  (test-case "missing observations cannot overwrite a later queued recreation"
    (define probes (make-async-channel))
    (with-session (controlled-controller probes)
                  (lambda (session)
                    (define path (build-path scratch "recreated.rkt"))
                    (define deleted (refresh session path))
                    (define read-1 (await probes))
                    (define recreated (refresh session path))
                    (controller-barrier session)
                    (release! read-1 (Disk-Missing))
                    (check-eq? 'file-not-found (Source-View-content-state (Source-Result-view (await deleted))))
                    (release! (await probes) "recreated")
                    (check-equal? "recreated" (result-text (await recreated)))
                    (check-eq? 'text-available
                               (Source-View-content-state (Source-Result-view (await recreated)))))))

  (test-case "arbitrary raise and premature reader exit settle and permit another refresh"
    (define probes (make-async-channel))
    (with-session (controlled-controller probes)
                  (lambda (session)
                    (define path (build-path scratch "read-failure.rkt"))
                    (for ([failure (in-list '(raise die))])
                      (define result (refresh session path))
                      (release! (await probes) failure)
                      (define failed (await result))
                      (check-eq? 'unknown (Source-View-content-state (Source-Result-view failed)))
                      (check-true (Source-Failure? (Source-Result-error failed))))
                    (define retry (refresh session path))
                    (release! (await probes) "recovered")
                    (check-equal? "recovered" (result-text (await retry))))))

  (test-case "read timeout settles the active read and starts its pending refresh"
    (define probes (make-async-channel))
    (with-session (controlled-controller probes #:timeout 0.1)
                  (lambda (session)
                    (define path (build-path scratch "timeout.rkt"))
                    (define first (refresh session path))
                    (await probes)
                    (define queued (refresh session path))
                    (controller-barrier session)
                    (check-equal? 'read-timeout
                                  (Source-Failure-reason (Source-Result-error (await first))))
                    (release! (await probes) "later")
                    (check-equal? "later" (result-text (await queued))))))

  (test-case "failed reads discard cached text and keep failure details in the result"
    (define probes (make-async-channel))
    (with-session (controlled-controller probes)
                  (lambda (session)
                    (define path (build-path scratch "cached-read-failure.rkt"))
                    (define first (refresh session path))
                    (release! (await probes) "cached")
                    (define previous (Source-Result-view (await first)))
                    (for ([_ (in-range 2)])
                      (define next (refresh session path))
                      (release! (await probes) 'raise)
                      (define failed (await next))
                      (define current (Source-Result-view failed))
                      (check-eq? 'unknown (Source-View-content-state current))
                      (check-false (Source-Result-snapshot failed))
                      (check-equal? (Source-Result-error failed)
                                    (Source-Failure 'arbitrary-read-failure))
                      (check-not-false (sync/timeout 0 (Source-View-changed previous)))
                      (set! previous current)))))

  (test-case "read errors leave content unknown and report a separate failure"
    (with-files
      (lambda (root)
        (with-session (make-source-controller)
                      (lambda (session)
                        (define failed (await (refresh session root))) ; reading a directory fails
                        (check-eq? 'unknown (Source-View-content-state (Source-Result-view failed)))
                        (check-true (Source-Failure? (Source-Result-error failed))))))))

  (test-case "folder removal preserves a held source and rename invalidates both paths"
    (define probes (make-async-channel))
    (with-session (controlled-controller probes)
                  (lambda (session)
                    (define old-path (build-path scratch "rename-old.rkt"))
                    (define new-path (build-path scratch "rename-new.rkt"))
                    (define folder (hasheq 'uri (path->uri scratch) 'name "scratch"))
                    (notify session "workspace/didChangeWorkspaceFolders"
                            (hasheq 'event (hasheq 'added (list folder) 'removed '())))
                    (barrier session)
                    (define old-result (refresh session old-path))
                    (define old-read (await probes))
                    (notify session "workspace/didChangeWorkspaceFolders"
                            (hasheq 'event (hasheq 'added '() 'removed (list folder))))
                    (barrier session)
                    (check-false (sync/timeout 0 old-result))
                    (define new-result (refresh session new-path))
                    (define new-read (await probes))
                    (notify session "workspace/didRenameFiles"
                            (hasheq 'files (list (hasheq 'oldUri (path->uri old-path)
                                                         'newUri (path->uri new-path)))))
                    (barrier session)
                    (for ([result (in-list (list (await old-result) (await new-result)))])
                      (check-eq? 'unknown (Source-View-content-state (Source-Result-view result)))
                      (check-false (Source-Result-snapshot result)))
                    (release! old-read "obsolete old")
                    (release! new-read "obsolete destination")
                    (define next (refresh session new-path))
                    (release! (await probes) "renamed")
                    (check-equal? "renamed" (result-text (await next))))))

  (test-case "captured source view changes under edit/close and close signals every waiter"
    (with-session (make-source-controller)
                  (lambda (session)
                    (define path (build-path scratch "captured.rkt"))
                    (open! session path "#lang racket/base\n" 4)
                    (barrier session)
                    (define captured (view session path))
                    (check-eq? captured (view session path))
                    (notify session "textDocument/didSave"
                            (hasheq 'textDocument (hasheq 'uri (path->uri path)) 'text "disk text"))
                    (barrier session)
                    (check-eq? captured (view session path) "saving leaves buffer authority intact")
                    (notify session "textDocument/didChange"
                            (hasheq 'textDocument (hasheq 'uri (path->uri path) 'version 5)
                                    'contentChanges
                                    (list (hasheq 'text "#lang racket/base\n; edit\n"))))
                    (barrier session)
                    (check-not-false (sync/timeout 0 (Source-View-changed captured)))
                    (define edited (view session path))
                    (close! session path)
                    (barrier session)
                    (check-not-false (sync/timeout 0 (Source-View-changed edited)))
                    (define query-path (build-path scratch "close-query.rkt"))
                    (open! session query-path "#lang racket/base\n")
                    (barrier session)
                    (define pending (Source-View-changed (view session query-path)))
                    (close! session query-path)
                    (barrier session)
                    (check-not-false (await pending))
                    ;; The broadcast stays ready for a second waiter after close.
                    (check-not-false (await pending)))))

  (test-case "shutdown settles held, queued, and subsequent observations"
    (define probes (make-async-channel))
    (with-session (controlled-controller probes)
                  (lambda (session)
                    (define path (build-path scratch "shutdown.rkt"))
                    (define held (refresh session path))
                    (await probes)
                    (define queued (refresh session path))
                    (controller-barrier session)
                    (async-channel-put (Session-clients session)
                                       (hasheq 'id "shutdown" 'method "shutdown"))
                    (response session "shutdown")
                    (for ([event (in-list (list held queued
                                                (refresh session path)))])
                      (check-equal? (Source-Failure 'shutdown) (await event)))))))
