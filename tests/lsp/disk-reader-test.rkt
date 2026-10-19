#lang racket/base

(require rackunit
         racket/async-channel
         "../../common/source.rkt"
         "../../workspace/source-store.rkt"
         "../../lsp/source-controller/completion-event.rkt"
         "../../lsp/source-controller/disk-reader.rkt")

(module+ test
  (define source (path->source (string->path "/disk-reader-test.rkt")))
  (define (view)
    (Source-View source (gensym 'revision) 'disk 'unknown never-evt))
  (define (await event)
    (define result (sync/timeout 3 (wrap-evt event box)))
    (unless result (error 'await "event did not become ready"))
    (unbox result))
  (define (next-message events)
    (define message (await events))
    (check-pred Disk-Read-Completed? message)
    message)

  (define (with-reader events read proc)
    (define custodian (make-custodian))
    (define reader (make-disk-reader events #:read read))
    (dynamic-wind
      void
      (lambda () (parameterize ([current-custodian custodian]) (proc reader)))
      (lambda ()
        (stop-disk-reader! reader 'shutdown)
        (custodian-shutdown-all custodian))))

  (test-case "queued commands share successor read and capture accepted successor view"
    (define events (make-async-channel))
    (define calls (make-async-channel))
    (define replies (make-async-channel))
    (with-reader
      events
      (lambda (path) (async-channel-put calls path) (async-channel-get replies))
      (lambda (reader)
        (define initial (view))
        (define successor (view))
        (define first (request-disk-read! reader initial))
        (check-equal? (await calls) (source-file-path source))
        (define second (request-disk-read! reader initial))
        (check-eq? second (request-disk-read! reader initial))
        (check-not-eq? first second)
        (async-channel-put replies (string-copy "first"))
        (define message (next-message events))
        (finish-disk-read!
          reader message
          (lambda (started result)
            (check-eq? started initial)
            (check-equal? result "first")
            (check-true (immutable? result))
            (check-false (evt-ready? first))
            (check-false (evt-ready? second))
            (values 'accepted successor)))
        (check-eq? (await first) 'accepted)
        (check-equal? (await calls) (source-file-path source))
        (check-false (evt-ready? second))
        (async-channel-put replies "second")
        (finish-disk-read!
          reader (next-message events)
          (lambda (started result)
            (check-eq? started successor)
            (check-equal? result "second")
            (values 'successor-accepted successor)))
        (check-eq? (await second) 'successor-accepted))))

  (test-case "cancel answers both generations and rejects stale completion against a new lane"
    (define events (make-async-channel))
    (with-reader
      events (lambda (_) "disk")
      (lambda (reader)
        (define initial (view))
        (define first (request-disk-read! reader initial))
        (define pending (request-disk-read! reader initial))
        ;; Retain an actual worker message so replay exercises attempt identity.
        (define stale (next-message events))
        (cancel-disk-read! reader source 'cancelled)
        (check-eq? (await first) 'cancelled)
        (check-eq? (await pending) 'cancelled)
        (define fresh (request-disk-read! reader initial))
        (finish-disk-read! reader stale (lambda (_started _result)
                                          (fail "cancelled observation was accepted")
                                          (values #f initial)))
        (check-false (evt-ready? fresh))
        (finish-disk-read! reader (next-message events)
                           (lambda (_started _result) (values 'fresh initial)))
        (check-eq? (await fresh) 'fresh)
        (stop-disk-reader! reader 'shutdown)
        (check-eq? (await (request-disk-read! reader initial)) 'shutdown))))

  (test-case "malformed injected results become disk failures"
    (define events (make-async-channel))
    (with-reader
      events (lambda (_) '(bad result))
      (lambda (reader)
        (define initial (view))
        (define completion (request-disk-read! reader initial))
        (finish-disk-read! reader (next-message events)
                           (lambda (_started result)
                             (check-equal? result (Disk-Read-Failure '(bad result)))
                             (values 'failed initial)))
        (check-eq? (await completion) 'failed)))))
