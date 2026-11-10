#lang racket/base

;; Exercise retained visuals with real analysis and edit-adjusted Doc services.
(require rackunit
         racket/async-channel
         racket/class
         "../../common/interfaces.rkt"
         "../../common/path-util.rkt"
         "../../common/source.rkt"
         "../../common/time.rkt"
         "../../doclib/doc.rkt"
         "../../lsp/analysis.rkt"
         "../../lsp/analysis-query.rkt"
         "../../lsp/methods.rkt"
         "../../lsp/safedoc.rkt"
         "../../lsp/source-controller.rkt"
         (submod "../../lsp/source-controller.rkt" owner)
         "controller-support.rkt")

(define text "#lang racket\n(struct point (x y))\n(point 1 2)\n(point 3 4)\n")
(define whole (Range (Pos 0 0) (Pos 50 0)))

(module+ test
  (test-case "failed expansion keeps shifted visuals, drops touched groups, and repair replaces them"
    (define gates (make-async-channel))
    (define controller
      (make-source-controller
        #:resyntax #f
        #:analyze (lambda (snapshot)
                    (define command (wait-at-gate gates snapshot))
                    (if (eq? command 'raise)
                        (raise 'unexpected-worker-error)
                        (analyze-source snapshot)))))
    (define path (build-path scratch "stable-visuals.rkt"))
    (define source (path->source path))
    (define sd (new-safedoc (path->uri path) text 0))
    (define (completion) (with-read-safedoc sd SafeDoc-analysis-completion))
    (define (finish! [command 'go])
      (release-gate! (await gates) command)
      (drive-controller controller (completion) 90))
    (define (query reader)
      (define result (visual-query sd 1 'inlay-hints reader))
      (if (procedure? result) (result) result))
    (define (hints)
      (hash-ref (Visual-Delivery-response (query (lambda (doc) (doc-inlay-hints doc whole))))
                'result))
    (define (hint-lines)
      (map (lambda (hint) (hash-ref (hash-ref hint 'position) 'line)) (hints)))
    (define (edit! start end replacement)
      (source-controller-edit!
        controller sd (lambda (doc) (doc-apply-edit! doc (Range start end) replacement))))

    (parameterize ([current-source-controller controller])
      (dynamic-wind
        (lambda () (source-controller-open! controller source sd))
        (lambda ()
          (finish!)
          (check-equal? (hint-lines) '(2 2 3 3))
          (define original-tokens
            (with-read-doc sd (lambda (doc) (doc-range-tokens doc whole))))
          (edit! (Pos 1 0) (Pos 1 0) "(define\n")
          (define gate (await gates))
          ;; Deadline fallback leaves the shared worker alive.
          (define fallback
            (parameterize ([current-analysis-request-timeout 0])
              (query (lambda (doc) (doc-inlay-hints doc whole)))))
          (check-true (Visual-Delivery? fallback))
          (check-not-false (Visual-Delivery-degraded-view fallback))
          (check-false (sync/timeout 0 (completion)))
          (define broken-reader (visual-query sd 3 'inlay-hints
                                              (lambda (_) (raise 'reader-failed))))
          (release-gate! gate)
          (check-eq? (Analysis-Outcome-state (drive-controller controller (completion) 90)) 'errors)
          (check-error (broken-reader) ErrorCode-RequestFailed)
          (check-false (with-read-safedoc sd SafeDoc-available))
          (check-equal? (hint-lines) '(3 3 4 4))
          (define adjusted-tokens
            (with-read-doc sd (lambda (doc) (doc-range-tokens doc whole))))
          (check-equal?
            adjusted-tokens
            (for/list ([token (in-list original-tokens)])
              (if (>= (SemanticToken-start token) (string-length "#lang racket\n"))
                  (struct-copy SemanticToken token
                    [start (+ 8 (SemanticToken-start token))]
                    [end (+ 8 (SemanticToken-end token))])
                  token)))
          (edit! (Pos 3 7) (Pos 3 8) "100")
          (finish!)
          (check-equal? (hint-lines) '(4 4))
          (source-controller-edit! controller sd (lambda (doc) (doc-reset! doc text)))
          (finish!)
          (check-equal? (hint-lines) '(2 2 3 3))
          ;; A ready worker failure wins over an expired request budget.
          (source-controller-refresh-analysis! controller source)
          (finish! 'raise)
          (parameterize ([current-analysis-request-timeout 0])
            (check-error (query (lambda (_) (error 'reader "must not read")))
                         ErrorCode-RequestFailed)))
        (lambda () (source-controller-shutdown! controller)))))

  (test-case "consumer cancels captured visual deliveries after edit or close"
    (define controller (make-source-controller #:resyntax #f))
    (define responses (make-async-channel))
    (define server
      (new server% [source-controller controller]
        [response-channel responses]
        [request-channel (make-async-channel)]
        [notification-channel (make-async-channel)]))
    (define path (build-path scratch "visual-delivery.rkt"))
    (define source (path->source path))
    (define sd (new-safedoc (path->uri path) text 0))
    (parameterize ([current-source-controller controller]
                   [current-analysis-request-timeout 0])
      (dynamic-wind
        (lambda () (source-controller-open! controller source sd))
        (lambda ()
          (define (capture) (visual-query sd 2 'semantic-tokens (lambda (_) (hasheq 'data '()))))
          (define edited (capture))
          (source-controller-edit! controller sd (lambda (doc) (doc-update-version! doc 1)))
          (send server process-source-event edited)
          (check-error (await responses) ErrorCode-ContentModified)
          (define closed (capture))
          (source-controller-close! controller source sd)
          (send server process-source-event closed)
          (check-error (await responses) ErrorCode-ContentModified)
          (check-false (sync/timeout 0 responses)))
        (lambda () (source-controller-shutdown! controller)))))

  (test-case "immediate and deferred protocol results queue exactly one consumer delivery"
    (define gates (make-async-channel))
    (define controller
      (make-source-controller #:resyntax #f
                              #:analyze (lambda (snapshot)
                                          (wait-at-gate gates snapshot)
                                          (analyze-source snapshot))))
    (define responses (make-async-channel))
    (define server
      (new server% [source-controller controller]
        [response-channel responses]
        [request-channel (make-async-channel)]
        [notification-channel (make-async-channel)]))
    (define uri "untitled:visual-routing")
    (define (request id)
      (send server process-message
            (hasheq 'id id 'method "textDocument/semanticTokens/full"
                    'params (hasheq 'textDocument (hasheq 'uri uri)))))
    (dynamic-wind
      void
      (lambda ()
        (send server process-message
              (hasheq 'method "textDocument/didOpen"
                      'params (hasheq 'textDocument
                                      (hasheq 'uri uri 'languageId "racket"
                                              'version 0 'text text))))
        (define gate (await gates))
        (define worker (request 10))
        (check-true (thread? worker))
        (check-false (sync/timeout 0 responses))
        (release-gate! gate)
        ;; Accept the completed worker product; the deferred reader then queues delivery.
        (send server process-source-event (await (source-controller-events controller) 90))
        (define delivery (await (source-controller-events controller)))
        (check-true (Visual-Delivery? delivery))
        (await (thread-dead-evt worker))
        (check-false (sync/timeout 0 responses))
        (send server process-source-event delivery)
        (check-equal? (hash-ref (await responses) 'id) 10)
        (check-false (sync/timeout 0 responses))
        ;; Installed analysis now takes the immediate path through the same queue.
        (request 11)
        (define immediate (await (source-controller-events controller)))
        (check-true (Visual-Delivery? immediate))
        (check-false (sync/timeout 0 responses))
        (send server process-source-event immediate)
        (check-equal? (hash-ref (await responses) 'id) 11)
        (check-false (sync/timeout 0 responses)))
      (lambda () (source-controller-shutdown! controller))))

  (test-case "replacement completion keeps the original budget and exposes ready failures"
    (define gates (make-async-channel))
    (define controller
      (make-source-controller
        #:resyntax #f
        #:analyze (lambda (snapshot)
                    (if (eq? (wait-at-gate gates snapshot) 'raise)
                        (raise 'replacement-failed)
                        (analyze-source snapshot)))))
    (define source (path->source (build-path scratch "visual-replacement.rkt")))
    (define sd (new-safedoc (path->uri (build-path scratch "visual-replacement.rkt"))
                            "#lang racket/base\n(define" 0))
    (define (completion) (with-read-safedoc sd SafeDoc-analysis-completion))
    (parameterize ([current-source-controller controller])
      (dynamic-wind
        (lambda () (source-controller-open! controller source sd))
        (lambda ()
          (define first (await gates))
          (define deferred
            (parameterize ([current-analysis-request-timeout 0.1])
              (visual-query sd 4 'semantic-tokens (lambda (_) (hasheq 'data '())))))
          (define expired (deadline-evt (+ (current-deadline-milliseconds) 100)))
          (release-gate! first)
          (check-eq? 'errors (Analysis-Outcome-state (drive-controller controller (completion) 90)))
          (source-controller-refresh-analysis! controller source)
          (define replacement (await gates))
          (sync expired)
          ;; Even invoking under a longer budget cannot restart the captured one.
          (define fallback
            (parameterize ([current-analysis-request-timeout 30]) (deferred)))
          (check-true (Visual-Delivery? fallback))
          (check-false (sync/timeout 0 (completion)))
          (release-gate! replacement 'raise)
          (drive-controller controller (completion))
          ;; The same deferred reader polls the replacement failure before expiry.
          (check-error (deferred) ErrorCode-RequestFailed)
          ;; A second request follows a replaced, completed error to the new failure.
          (source-controller-refresh-analysis! controller source)
          (define old (await gates))
          (define follow (visual-query sd 5 'inlay-hints (lambda (_) '())))
          (release-gate! old)
          (drive-controller controller (completion) 90)
          (source-controller-refresh-analysis! controller source)
          (define newest (await gates))
          (define result (make-async-channel))
          (thread (lambda () (async-channel-put result (follow))))
          (check-false (sync/timeout 0 result))
          (release-gate! newest 'raise)
          (drive-controller controller (completion))
          (check-error (await result) ErrorCode-RequestFailed))
        (lambda () (source-controller-shutdown! controller))))))
