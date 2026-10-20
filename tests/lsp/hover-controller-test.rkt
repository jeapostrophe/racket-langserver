#lang racket/base

(require "../../common/source.rkt"
         json
         rackunit
         racket/async-channel
         racket/file
         racket/string
         "../../common/interfaces.rkt"
         "../../common/json-util.rkt"
         "../../common/path-util.rkt"
         "../../doclib/doc.rkt"
         "../../doclib/hover.rkt"
         "../../lsp/analysis.rkt"
         "../../lsp/lsp.rkt"
         "../../lsp/safedoc.rkt"
         "../../lsp/source-controller.rkt"
         "../../workspace/source-store.rkt"
         (submod "source-controller-test.rkt" test-support)
         (submod "definition-controller-test.rkt" test-support))

(define (request session id a [method "textDocument/hover"] [pos (Pos 3 7)])
  (async-channel-put (Session-clients session)
                     (hasheq 'id id 'method method
                             'params (hasheq 'textDocument (hasheq 'uri (path->uri a))
                                             'position (->jsexpr pos)))))

(define (check-hover session id text)
  (define result (hash-ref (response session id) 'result))
  (check-equal? (hash-ref result 'range) (->jsexpr (Range (Pos 3 6) (Pos 3 12))))
  (define contents (hash-ref result 'contents))
  (check-true (string-contains? contents "**Source**"))
  (check-true (string-contains? contents text) contents)
  (check-true (string-contains? contents "**Mouse-over status**"))
  (check-true (string-contains? contents "imported from"))
  result)

(module+ test
  (test-case "documented imported hover skips target reads and analysis"
    (make-directory* scratch)
    (define a (make-temporary-file "documented-hover~a.rkt" #f scratch))
    (define starts (make-async-channel))
    (define reads (make-async-channel))
    (define text "#lang racket/base\n(list 1)\n")
    (dynamic-wind
      (lambda () (display-to-file text a #:exists 'truncate))
      (lambda ()
        (with-session
          (make-source-controller
            #:resyntax #f
            #:read (lambda (path)
                     (async-channel-put reads path)
                     (file->string path))
            #:analyze (lambda (snapshot)
                        (async-channel-put starts (Source-Snapshot-id snapshot))
                        (analyze-source snapshot)))
          (lambda (session)
            (open! session a text)
            (barrier session)
            (await (acquire session a))
            (check-equal? (await starts) (source-id (path->source a)))
            (request session 1 a "textDocument/hover" (Pos 1 2))
            (define result (hash-ref (response session 1) 'result))
            (define contents (hash-ref result 'contents))
            (check-true (string-contains? contents "**Documentation**"))
            (check-true (string-contains? contents "**Signature**"))
            (check-true (string-contains? contents "imported from"))
            (check-false (string-contains? contents "**Source**"))
            (check-equal? (hash-ref result 'range)
                          (->jsexpr (Range (Pos 1 1) (Pos 1 5))))
            (check-false (sync/timeout 0 reads))
            (check-false (sync/timeout 0 starts))
            (close! session a)
            (barrier session))))
      (lambda () (delete-file a))))

  (test-case "definition and hover share held analysis and alternating cached reads"
    (define gates (make-async-channel))
    (with-example
      (gated-analyzer gates)
      (lambda (session a b _root)
        (request session 1 a)
        (define held (await gates))
        (request session 2 a "textDocument/definition")
        (request session 3 a)
        (request session 4 a "textDocument/hover" (Pos 3 14))
        (check-true (string-contains? (hash-ref (hash-ref (response session 4) 'result)
                                                'contents) "(define local 1)"))
        (release! held)
        (check-hover session 1 "(define shared 1)")
        (check-location session 2 b)
        (check-hover session 3 "(define shared 1)")
        (request session 5 a "textDocument/definition")
        (check-location session 5 b)
        (request session 6 a)
        (check-hover session 6 "(define shared 1)")
        (check-false (sync/timeout 0 gates)))))

  (test-case "open unsaved comments use the Check Syntax job; close restores disk detail"
    (define gates (make-async-channel))
    (with-example
      (gated-analyzer gates)
      (lambda (session a b _root)
        (open! session b unsaved 7)
        (define held (await gates))
        (request session 1 a)
        (barrier session)
        (release! held)
        (check-hover session 1 ";; Unsaved\n(define shared 2)")
        (request session 2 a "textDocument/definition")
        (check-location session 2 b 4)
        (check-false (sync/timeout 0 gates))
        (close! session b)
        (barrier session)
        (request session 3 a)
        (release! (await gates))
        (define restored (check-hover session 3 "(define shared 1)"))
        (check-false (string-contains? (hash-ref restored 'contents) "Unsaved")))))

  (test-case "hover observes changed disk text on the next query without a final reread"
    (define reads (make-async-channel))
    (with-example
      (gated-reader reads)
      (lambda (session a b _root)
        (request session 1 a)
        (release! (await reads))
        (check-hover session 1 "(define shared 1)")
        (check-false (sync/timeout 0 reads))
        (display-to-file unsaved b #:exists 'truncate)
        (request session 2 a)
        (release! (await reads))
        (check-hover session 2 ";; Unsaved\n(define shared 2)")
        (check-false (sync/timeout 0 reads)))))

  (test-case "opening the target ends an obsolete hover without waiting for new analysis"
    (define gates (make-async-channel))
    (with-example
      (gated-analyzer gates)
      (lambda (session a b _root)
        (request session 1 a)
        (await gates)
        (open! session b unsaved)
        (define fresh (await gates))
        (check-error session 1 ErrorCode-RequestFailed "Hover target changed")
        (release! fresh)
        (request session 2 a)
        (check-hover session 2 ";; Unsaved\n(define shared 2)")
        (close! session b)
        (barrier session))))

  (for ([operation (in-list '(edit close))])
    (test-case (format "A ~a cancels hover before held analysis is released" operation)
      (define gates (make-async-channel))
      (with-example
        (gated-analyzer gates)
        (lambda (session a _b _root)
          (request session 1 a)
          (define held (await gates))
          (if (eq? operation 'edit) (edit! session a (file->string a)) (close! session a))
          (barrier session)
          (check-error session 1 ErrorCode-ContentModified)
          (release! held)))))

  (test-case "missing and unsuccessful targets retain A's annotation"
    (with-example plain-controller
                  (lambda (session a b _root)
                    (delete-file b)
                    (request session 1 a)
                    (define missing (hash-ref (response session 1) 'result))
                    (check-true (string-contains? (hash-ref missing 'contents) "imported from"))
                    (check-false (string-contains? (hash-ref missing 'contents) "**Source**"))
                    (display-to-file "#lang racket/base\n(" b)
                    (request session 2 a)
                    (check-equal? (hash-ref (response session 2) 'result) missing))))

  (test-case "runner failure produces RequestFailed; later hover retries"
    (define gates (make-async-channel))
    (with-example
      (gated-analyzer gates)
      (lambda (session a _b _root)
        (request session 1 a)
        (release! (await gates) 'raise)
        (check-error session 1 ErrorCode-RequestFailed)
        (request session 2 a)
        (release! (await gates))
        (check-hover session 2 "(define shared 1)"))))

  (test-case "Typed Racket type survives joining imported source detail"
    (with-example
      plain-controller
      (lambda (session a _b _root)
        (request session 1 a)
        ;; A's inferred type must survive composition with B's source detail.
        (define result (check-hover session 1 "(define shared 1)"))
        (check-true (string-contains? (hash-ref result 'contents) "**Type**")))
      "#lang typed/racket/base\n(require \"b.rkt\")\n(define local 1)\n(list shared local)\n"
      ;; Older Typed Racket can report contracted imports under generated names
      ;; while the target declaration keeps its original name.
      #:target-text (string-append "#lang typed/racket/base\n(require typed/racket/unsafe)\n"
                                   "(unsafe-provide shared)\n(define shared 1)\n")))

  (test-case "virtual imported hover stays document-local without target reads"
    (with-example
      (lambda (_b) (make-source-controller #:resyntax #f
                                           #:read (lambda (_) (error 'disk "unexpected read"))))
      (lambda (session a _b _root)
        (define uri "untitled:imported-hover")
        (notify session "textDocument/didOpen"
                (hasheq 'textDocument (hasheq 'uri uri 'languageId "racket"
                                              'version 0 'text (file->string a))))
        (barrier session)
        (await (acquire session (uri->source uri)))
        (async-channel-put (Session-clients session)
                           (hasheq 'id 1 'method "textDocument/hover"
                                   'params (hasheq 'textDocument (hasheq 'uri uri)
                                                   'position (->jsexpr (Pos 3 7)))))
        (define contents (hash-ref (hash-ref (response session 1) 'result) 'contents))
        (check-true (string-contains? contents "imported from"))
        (check-false (string-contains? contents "**Source**"))
        (notify session "textDocument/didClose" (hasheq 'textDocument (hasheq 'uri uri)))
        (barrier session))))

  (test-case "closed facts contain bounded immutable summary text"
    (with-example
      plain-controller
      (lambda (session a b _root)
        (request session 1 a)
        (check-hover session 1 "(define shared 1)")
        (define outcome (await (acquire session b)))
        (define summaries (Analysis-Summary-hover-summaries (Analysis-Outcome-value outcome)))
        (check-true (immutable? summaries))
        (for ([summary (in-hash-values summaries)])
          (check-true (immutable? (Hover-Code-Summary-text summary)))
          (check-true (immutable? (Hover-Code-Summary-fence-language summary))))))))

(module+ test
  (test-case "closed target hover returns retained source detail before refresh completes"
    (define gates (make-async-channel))
    (with-example
      (gated-analyzer gates)
      (lambda (session a b _root)
        (open! session b saved 7)
        (release! (await gates))
        (barrier session)
        (await (acquire session b))
        (close! session b)
        (barrier session)
        (request session 301 a)
        (define held (await gates))
        (check-hover session 301 "(define shared 1)")
        (request session 302 a)
        (check-hover session 302 "(define shared 1)")
        (check-false (sync/timeout 0 gates))
        (release! held)))))
