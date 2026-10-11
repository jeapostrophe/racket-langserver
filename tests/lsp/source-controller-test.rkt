#lang racket/base

;; Source-controller checks through the production consumer. The test holds
;; disk reads, so ordering does not depend on sleeps. A direct process-message
;; call would skip the client and internal event pump.
(require json
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
         "../../doclib/doc.rkt"
         "../../lsp/lsp.rkt"
         "../../lsp/methods.rkt"
         "../../lsp/safedoc.rkt"
         "../../lsp/scheduler.rkt"
         "../../lsp/source-controller.rkt"
         "../../workspace/source-store.rkt")

(define-runtime-path scratch-path "../../.agent-scratch")

(define scratch (simplify-path scratch-path #f))

(struct/contract Probe ([path path?] [release async-channel?]) #:transparent)

(struct/contract Session
  ([controller Source-Controller?]
   [clients async-channel?]
   [responses async-channel?]
   [notifications async-channel?])
  #:transparent)

(define (await event)
  (define value (sync/timeout 5 event))
  (unless value (error 'await "event did not complete within 5 seconds"))
  value)

(define (owner session proc)
  (await (source-controller-call-evt (Session-controller session) proc)))

(define (view session path)
  (owner session (lambda (controller) (source-controller-view controller path))))

(define (refresh session path)
  (source-controller-refresh-evt (Session-controller session) path))

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
;; Owner commands are a separate queue and cannot serve that purpose.
(define (barrier session)
  (async-channel-put (Session-clients session)
                     (hasheq 'id 100 'method "textDocument/hover"
                             'params (hasheq)))
  (response session 100))

(define (response session id)
  (define deadline (+ (current-inexact-monotonic-milliseconds) 5000))
  (let loop ()
    (define msg
      (sync/timeout (max 0 (/ (- deadline (current-inexact-monotonic-milliseconds)) 1000))
                    (Session-responses session)))
    (unless msg (error 'response "no response for id ~a within 5 seconds" id))
    (if (equal? (hash-ref msg 'id #f) id) msg (loop))))

(define (with-session controller proc)
  (define custodian (make-custodian))
  (define clients (make-async-channel))
  (define responses (make-async-channel))
  (define notifications (make-async-channel))
  (define session (Session controller clients responses notifications))
  (parameterize ([current-custodian custodian])
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
      (owner session (lambda (controller) (source-controller-shutdown! controller)))
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

(module+ test
  (test-case "controller contracts reject invalid readers, timeouts, and results"
    (check-exn exn:fail:contract? (lambda () (make-source-controller #:read #f)))
    (check-exn exn:fail:contract? (lambda () (make-source-controller #:read (lambda () "text"))))
    (check-exn exn:fail:contract? (lambda () (make-source-controller #:read-timeout -1)))
    (check-exn exn:fail:contract? (lambda () (Source-Result #f #f #f)))
    (define view (source-store-view (make-source-store) (build-path scratch "contract.rkt")))
    (check-exn exn:fail:contract? (lambda () (Source-Result view #f 'invalid-error))))

  (test-case "SafeDoc source-view mutations enforce their contract"
    (with-session (make-source-controller)
                  (lambda (session)
                    (define path (build-path scratch "contract-buffer.rkt"))
                    (open! session path "#lang racket/base\n")
                    (barrier session)
                    (owner session
                           (lambda (_)
                             (with-write-safedoc (lsp-get-doc (path->uri path))
                               (lambda (sd)
                                 (define before (SafeDoc-source-view sd))
                                 (check-exn exn:fail:contract?
                                            (lambda () (set-SafeDoc-source-view! sd 'invalid)))
                                 (check-eq? before (SafeDoc-source-view sd)))))))))

  (test-case "ordered refresh barriers cannot reuse a read started before the command"
    (define probes (make-async-channel))
    (with-session (controlled-controller probes)
                  (lambda (session)
                    (define path (build-path scratch "ordered.rkt"))
                    (define first (refresh session path))
                    (define read-1 (await probes))
                    (define second (refresh session path))
                    (define third (refresh session path))
                    (owner session void) ; internal FIFO has accepted both pending refreshes
                    (release! read-1 "old")
                    (check-equal? "old" (result-text (await first)))
                    (define old-view (view session path))
                    (define read-2 (await probes))
                    (check-false (sync/timeout 0 second))
                    (release! read-2 "new")
                    (define result-2 (await second))
                    (define result-3 (await third))
                    (check-eq? result-2 (await second)
                               "a completed batch is observable without consumption")
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
                    (check-equal? (owner session
                                         (lambda (_)
                                           (with-read-doc (lsp-get-doc (path->uri a))
                                             Doc-version)))
                                  1)
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
                    (define closed (view session path))
                    (check-eq? 'unobserved (Source-View-state closed))
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
                        (check-eq? 'missing (Source-View-state (Source-Result-view missing)))
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
                    (owner session void)
                    (release! read-1 (Disk-Missing))
                    (check-eq? 'missing (Source-View-state (Source-Result-view (await deleted))))
                    (release! (await probes) "recreated")
                    (check-equal? "recreated" (result-text (await recreated)))
                    (check-eq? 'present (Source-View-state (view session path))))))

  (test-case "arbitrary raise and premature reader exit settle and permit another refresh"
    (define probes (make-async-channel))
    (with-session (controlled-controller probes)
                  (lambda (session)
                    (define path (build-path scratch "read-failure.rkt"))
                    (for ([failure (in-list '(raise die))])
                      (define result (refresh session path))
                      (release! (await probes) failure)
                      (define failed (await result))
                      (check-eq? 'unavailable (Source-View-state (Source-Result-view failed)))
                      (check-true (Source-Failure? (Source-Result-error failed))))
                    (define retry (refresh session path))
                    (release! (await probes) "recovered")
                    (check-equal? "recovered" (result-text (await retry))))))

  (test-case "read timeout settles the batch and starts its pending refresh"
    (define probes (make-async-channel))
    (with-session (controlled-controller probes #:timeout 0.1)
                  (lambda (session)
                    (define path (build-path scratch "timeout.rkt"))
                    (define first (refresh session path))
                    (await probes)
                    (define queued (refresh session path))
                    (owner session void)
                    (check-equal? 'read-timeout
                                  (Source-Failure-reason (Source-Result-error (await first))))
                    (release! (await probes) "later")
                    (check-equal? "later" (result-text (await queued))))))

  (test-case "read errors are unavailable rather than missing"
    (with-files
      (lambda (root)
        (with-session (make-source-controller)
                      (lambda (session)
                        (define failed (await (refresh session root))) ; reading a directory fails
                        (check-eq? 'unavailable (Source-View-state (Source-Result-view failed)))
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
                    (define before-removal (view session old-path))
                    (notify session "workspace/didChangeWorkspaceFolders"
                            (hasheq 'event (hasheq 'added '() 'removed (list folder))))
                    (barrier session)
                    (check-eq? before-removal (view session old-path))
                    (check-false (sync/timeout 0 old-result))
                    (define new-result (refresh session new-path))
                    (define new-read (await probes))
                    (define before-rename (view session new-path))
                    (notify session "workspace/didRenameFiles"
                            (hasheq 'files (list (hasheq 'oldUri (path->uri old-path)
                                                         'newUri (path->uri new-path)))))
                    (barrier session)
                    (check-not-false (sync/timeout 0 (Source-View-changed before-removal)))
                    (check-not-false (sync/timeout 0 (Source-View-changed before-rename)))
                    (for ([result (in-list (list (await old-result) (await new-result)))])
                      (check-eq? 'unobserved (Source-View-state (Source-Result-view result)))
                      (check-false (Source-Result-snapshot result)))
                    (release! old-read "obsolete old")
                    (release! new-read "obsolete destination")
                    (define next (refresh session new-path))
                    (release! (await probes) "renamed")
                    (check-equal? "renamed" (result-text (await next))))))

  (test-case "captured SafeDoc view changes under edit/close and old query cleanup remains"
    (with-session (make-source-controller)
                  (lambda (session)
                    (define path (build-path scratch "captured.rkt"))
                    (open! session path "#lang racket/base\n" 4)
                    (barrier session)
                    (define captured
                      (owner session
                             (lambda (_)
                               (with-read-safedoc (lsp-get-doc (path->uri path))
                                 SafeDoc-source-view))))
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
                    (define-values (edited old-doc)
                      (apply values
                             (owner session
                                    (lambda (_)
                                      (define doc (lsp-get-doc (path->uri path)))
                                      (with-read-safedoc doc
                                        (lambda (sd)
                                          (list (SafeDoc-source-view sd) sd)))))))
                    (close! session path)
                    (barrier session)
                    (check-not-false (sync/timeout 0 (Source-View-changed edited)))
                    (check-false (with-read-safedoc old-doc SafeDoc-source-view))
                    ;; This open skips Check Syntax, so nothing completes the legacy query
                    ;; before didClose. Close is what settles it.
                    (define query-path (build-path scratch "close-query.rkt"))
                    (define pending
                      (owner session
                             (lambda (_)
                               (define doc
                                 (lsp-open-doc! (path->uri query-path) "#lang racket/base\n" 0))
                               (with-read-safedoc doc
                                 (lambda (sd) (async-query-wait (SafeDoc-token sd) values))))))
                    (close! session query-path)
                    (barrier session)
                    (define result (make-async-channel))
                    (thread (lambda () (async-channel-put result (pending))))
                    (check-true (signal-doc-close? (await result))))))

  (test-case "shutdown settles held, queued, and subsequent owner operations"
    (define probes (make-async-channel))
    (with-session (controlled-controller probes)
                  (lambda (session)
                    (define path (build-path scratch "shutdown.rkt"))
                    (define held (refresh session path))
                    (await probes)
                    (define queued (refresh session path))
                    (owner session void)
                    (async-channel-put (Session-clients session)
                                       (hasheq 'id "shutdown" 'method "shutdown"))
                    (response session "shutdown")
                    (for ([event (in-list (list held queued
                                                (source-controller-call-evt
                                                  (Session-controller session)
                                                  void)
                                                (refresh session path)))])
                      (check-equal? (Source-Failure 'shutdown) (await event)))))))
