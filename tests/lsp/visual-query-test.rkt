#lang racket/base

;; Exercise retained visuals with real analysis and edit-adjusted Doc services.
(require rackunit
         racket/async-channel
         racket/class
         racket/match
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
         "../../workspace/source-store.rkt"
         "controller-support.rkt")

(define text "#lang racket\n(struct point (x y))\n(point 1 2)\n(point 3 4)\n")
(define whole (Range (Pos 0 0) (Pos 50 0)))

(struct Visual-Buffer (controller gates source safe-doc) #:transparent)

(define (visual-completion buffer)
  (with-read-safedoc (Visual-Buffer-safe-doc buffer) SafeDoc-analysis-completion))

(define (await-visual buffer)
  (drive-controller (Visual-Buffer-controller buffer) (visual-completion buffer) 90))

(define (finish-visual! buffer [command 'go])
  (release-gate! (await (Visual-Buffer-gates buffer)) command)
  (await-visual buffer))

(define (query-visual buffer reader)
  (define result (visual-query (Visual-Buffer-safe-doc buffer) 1 'inlay-hints reader))
  (if (procedure? result) (result) result))

(define (visual-hint-lines buffer)
  (define delivery (query-visual buffer (lambda (doc) (doc-inlay-hints doc whole))))
  (for/list ([hint (in-list (hash-ref (Visual-Delivery-response delivery) 'result))])
    (hash-ref (hash-ref hint 'position) 'line)))

(define (edit-visual! buffer start end replacement)
  (source-controller-edit!
    (Visual-Buffer-controller buffer) (Visual-Buffer-safe-doc buffer)
    (lambda (doc) (doc-apply-edit! doc (Range start end) replacement))))

(define (with-visual-buffer filename proc
                            #:text [buffer-text text]
                            #:analysis-timeout [analysis-timeout 90])
  (define gates (make-async-channel))
  (define controller
    (make-source-controller
      #:resyntax #f
      #:analysis-timeout analysis-timeout
      #:analyze (lambda (snapshot)
                  (if (eq? (wait-at-gate gates snapshot) 'raise)
                      (raise 'unexpected-worker-error)
                      (analyze-source snapshot)))))
  (define path (build-path scratch filename))
  (define source (path->source path))
  (define sd (new-safedoc (path->uri path) buffer-text 0))
  (parameterize ([current-source-controller controller])
    (dynamic-wind
      (lambda () (source-controller-open! controller source sd))
      (lambda () (proc (Visual-Buffer controller gates source sd)))
      (lambda () (source-controller-shutdown! controller)))))

(module+ test
  (test-case
    "failed expansion keeps shifted visuals, drops touched groups, and repair replaces them"
    (with-visual-buffer
      "stable-visuals.rkt"
      (lambda (buffer)
        (match-define (struct* Visual-Buffer ([controller controller] [safe-doc sd])) buffer)
        (finish-visual! buffer)
        (check-equal? (visual-hint-lines buffer) '(2 2 3 3))
        (define original-tokens
          (with-read-doc sd (lambda (doc) (doc-range-tokens doc whole))))
        (edit-visual! buffer (Pos 1 0) (Pos 1 0) "(define\n")
        (check-eq? (Analysis-Outcome-state (finish-visual! buffer)) 'errors)
        (check-false (with-read-safedoc sd SafeDoc-available))
        (check-equal? (visual-hint-lines buffer) '(3 3 4 4))
        (check-equal?
          (with-read-doc sd (lambda (doc) (doc-range-tokens doc whole)))
          (for/list ([token (in-list original-tokens)])
            (if (>= (SemanticToken-start token) (string-length "#lang racket\n"))
                (struct-copy SemanticToken token
                  [start (+ 8 (SemanticToken-start token))]
                  [end (+ 8 (SemanticToken-end token))])
                token)))
        (edit-visual! buffer (Pos 3 7) (Pos 3 8) "100")
        (check-eq? (Analysis-Outcome-state (finish-visual! buffer)) 'errors)
        (check-equal? (visual-hint-lines buffer) '(4 4))
        (source-controller-edit! controller sd (lambda (doc) (doc-reset! doc text)))
        (finish-visual! buffer)
        (check-equal? (visual-hint-lines buffer) '(2 2 3 3)))))

  (test-case "deadline fallback, already-settled errors, and failure precedence"
    (with-visual-buffer
      "visual-fallbacks.rkt"
      (lambda (buffer)
        (match-define (Visual-Buffer controller gates source sd) buffer)
        (finish-visual! buffer)
        (edit-visual! buffer (Pos 1 0) (Pos 1 0) "(define\n")
        (define gate (await gates))
        ;; Deadline fallback leaves the shared worker alive.
        (define fallback
          (parameterize ([current-analysis-request-timeout 0])
            (query-visual buffer (lambda (doc) (doc-inlay-hints doc whole)))))
        (check-true (Visual-Delivery? fallback))
        (check-true (Visual-Delivery-degraded? fallback))
        (check-false (sync/timeout 0 (visual-completion buffer)))
        (define broken-reader
          (visual-query sd 3 'inlay-hints (lambda (_) (raise 'reader-failed))))
        (release-gate! gate)
        (check-eq? (Analysis-Outcome-state (await-visual buffer)) 'errors)
        (check-error (broken-reader) ErrorCode-RequestFailed)
        ;; Settled errors return a degraded delivery immediately without deferring,
        ;; and immediate read failures still produce RequestFailed.
        (define settled
          (visual-query sd 4 'inlay-hints (lambda (doc) (doc-inlay-hints doc whole))))
        (check-true (Visual-Delivery? settled))
        (check-true (Visual-Delivery-degraded? settled))
        (check-equal? (visual-hint-lines buffer) '(3 3 4 4))
        (check-error (visual-query sd 5 'inlay-hints (lambda (_) (raise 'immediate-fail)))
                     ErrorCode-RequestFailed)
        ;; A ready worker failure wins over an expired request budget.
        (source-controller-refresh-analysis! controller source)
        (finish-visual! buffer 'raise)
        (parameterize ([current-analysis-request-timeout 0])
          (check-error (query-visual buffer (lambda (_) (error 'reader "must not read")))
                       ErrorCode-RequestFailed)))))

  (test-case "supervisor timeout clears exact availability and retains adjusted hints"
    (with-visual-buffer
      "visual-timeout.rkt"
      ;; Allow real analysis to finish before the second attempt is held until timeout.
      #:analysis-timeout 5
      (lambda (buffer)
        (match-define (Visual-Buffer controller gates source sd) buffer)
        (finish-visual! buffer)
        (check-not-false (with-read-safedoc sd SafeDoc-available))
        (check-equal? (visual-hint-lines buffer) '(2 2 3 3))
        (source-controller-refresh-analysis! controller source)
        (await gates)
        (check-eq? 'timed-out (Analysis-Outcome-state (await-visual buffer)))
        (check-false (with-read-safedoc sd SafeDoc-available))
        (define delivery
          (visual-query sd 6 'inlay-hints (lambda (doc) (doc-inlay-hints doc whole))))
        (check-true (Visual-Delivery? delivery))
        (check-true (Visual-Delivery-degraded? delivery))
        (check-equal?
          (for/list ([hint (in-list (hash-ref (Visual-Delivery-response delivery) 'result))])
            (hash-ref (hash-ref hint 'position) 'line))
          '(2 2 3 3)))))

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
        (request 10)
        (release-gate! gate)
        ;; Accept the completed worker product; the deferred reader then queues delivery.
        (send server process-source-event (await (source-controller-events controller) 90))
        (define delivery (await (source-controller-events controller)))
        (check-false (sync/timeout 0 responses))
        (send server process-source-event delivery)
        (check-equal? (hash-ref (await responses) 'id) 10)
        ;; Installed analysis now takes the immediate path through the same queue.
        (request 11)
        (define immediate (await (source-controller-events controller)))
        (check-false (sync/timeout 0 responses))
        (send server process-source-event immediate)
        (check-equal? (hash-ref (await responses) 'id) 11)
        (check-false (sync/timeout 0 responses)))
      (lambda () (source-controller-shutdown! controller))))

  (test-case "captured visual budget survives replacement attempts and cannot be extended"
    (with-visual-buffer
      "visual-budget.rkt"
      #:text "#lang racket/base\n(define"
      (lambda (buffer)
        (match-define (Visual-Buffer controller gates source sd) buffer)
        (define first (await gates))
        (define deferred
          (parameterize ([current-analysis-request-timeout 0.02])
            (visual-query sd 4 'semantic-tokens (lambda (_) (hasheq 'data '())))))
        (define expired (deadline-evt (+ (current-deadline-milliseconds) 25)))
        (release-gate! first)
        (check-eq? 'errors (Analysis-Outcome-state (await-visual buffer)))
        (source-controller-refresh-analysis! controller source)
        (await gates)
        (sync expired)
        ;; Invoking under a longer parameter cannot extend the captured deadline.
        (define fallback
          (parameterize ([current-analysis-request-timeout 30]) (deferred)))
        (check-true (Visual-Delivery? fallback))
        (check-false (sync/timeout 0 (visual-completion buffer))))))

  (test-case "deferred query exposes ready replacement failure and follows settled errors"
    (with-visual-buffer
      "visual-replacement.rkt"
      #:text "#lang racket/base\n(define"
      (lambda (buffer)
        (match-define (Visual-Buffer controller gates source sd) buffer)
        (define first (await gates))
        (define expired-query
          (parameterize ([current-analysis-request-timeout 0.02])
            (visual-query sd 4 'semantic-tokens (lambda (_) (hasheq 'data '())))))
        (define expired (deadline-evt (+ (current-deadline-milliseconds) 25)))
        (release-gate! first)
        (check-eq? 'errors (Analysis-Outcome-state (await-visual buffer)))
        (source-controller-refresh-analysis! controller source)
        (define replacement (await gates))
        (sync expired)
        (release-gate! replacement 'raise)
        (await-visual buffer)
        (check-error (expired-query) ErrorCode-RequestFailed)
        ;; A request captured before an error completes follows a newer refresh.
        (source-controller-refresh-analysis! controller source)
        (define old (await gates))
        (define follow (visual-query sd 5 'inlay-hints (lambda (_) '())))
        (release-gate! old)
        (await-visual buffer)
        (source-controller-refresh-analysis! controller source)
        (define newest (await gates))
        (define result (make-async-channel))
        (thread (lambda () (async-channel-put result (follow))))
        (check-false (sync/timeout 0 result))
        (release-gate! newest 'raise)
        (await-visual buffer)
        (check-error (await result) ErrorCode-RequestFailed)))))
