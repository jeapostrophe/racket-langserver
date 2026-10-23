#lang racket/base

;; Gates prove that restored editor results precede fresh expansion completion.
(require "../../common/source.rkt"
         rackunit
         racket/async-channel
         racket/file
         racket/list
         racket/path
         racket/runtime-path
         racket/string
         "../../common/interfaces.rkt"
         "../../common/json-util.rkt"
         "../../common/path-util.rkt"
         "../../common/settings.rkt"
         "../../lsp/analysis.rkt"
         "../../lsp/source-controller.rkt"
         "../../workspace/source-store.rkt"
         "controller-support.rkt")

(define (analyzer gates)
  (lambda (snapshot)
    (case (wait-at-gate gates snapshot)
      [(fail) (raise 'refresh-failed)]
      [(errors)
       (define product (analyze-source snapshot))
       (Analysis-Product #f #f (list (hasheq 'message "fresh analysis error"))
                         (Analysis-Product-trace product) #f)]
      [else (analyze-source snapshot)])))

;; Real expansion can exceed the short waits used for gate coordination.
(define (await-analysis event)
  (await event 90))

(define (open-analyzed! session gates path text [version 0])
  (open! session path text version)
  (release-gate! (await gates))
  (barrier session)
  (check-eq? 'succeeded (Analysis-Outcome-state (await-analysis (acquire session path)))))

(define (available session path)
  (define snapshot (Source-Result-snapshot (observation session path)))
  (Analysis-Handle-reusable-summary
    (await (source-controller-analysis-evt (Session-controller session) snapshot))))

(define (query session path id method [position (Pos 3 0)])
  (request! session id method path
            (hasheq 'position (->jsexpr position) 'range (->jsexpr (Range (Pos 0 0) (Pos 20 0)))))
  (hash-ref (response session id) 'result))

(define (with-files proc)
  (make-directory* scratch)
  (define root (make-temporary-file "closed-analysis~a" 'directory scratch))
  (dynamic-wind void (lambda () (proc root)) (lambda () (delete-directory/files root))))

(define (write-text path text)
  (display-to-file text path #:exists 'truncate))

(define typed-text
  "#lang typed/racket\n(struct point ([x : Integer] [y : Integer]))\n(define p (let ([unused 0]) (point 1 2)))\np\n")

(module+ test
  (test-case "real Typed Racket types, navigation, tokens, hints and diagnostics restore before refresh"
    (define gates (make-async-channel))
    (with-session (make-source-controller #:analyze (analyzer gates) #:resyntax #f)
                  (lambda (session)
                    (define path (build-path scratch "closed-typed.rkt"))
                    (open-analyzed! session gates path typed-text 7)
                    (define methods (list "textDocument/hover" "textDocument/definition"
                                          "textDocument/semanticTokens/full" "textDocument/inlayHint"))
                    (define results (for/list ([method (in-list methods)] [id (in-naturals 400)])
                                      (query session path id method)))
                    (check-true (string-contains? (hash-ref (first results) 'contents) "**Type**"))
                    (check-equal? (second results)
                                  (->jsexpr (Location (path->uri path) (Range (Pos 2 8) (Pos 2 9)))))
                    (check-true (pair? (hash-ref (third results) 'data)))
                    (check-true (pair? (fourth results)))
                    (define diagnostics (hash-ref (hash-ref (await (Session-notifications session)) 'params)
                                                  'diagnostics))
                    (check-not-false
                      (member "unused variable" (map (lambda (d) (hash-ref d 'message)) diagnostics)))
                    (close! session path)
                    (barrier session)
                    (await (Session-notifications session))
                    (open! session path typed-text 7)
                    (define held (await gates))
                    (barrier session)
                    (define fresh (acquire session path))
                    (for ([method (in-list methods)] [expected (in-list results)] [id (in-naturals 410)])
                      (check-equal? (query session path id method) expected))
                    (check-equal? (hash-ref (hash-ref (await (Session-notifications session)) 'params)
                                            'diagnostics) diagnostics)
                    (check-false (sync/timeout 0 fresh))
                    (release-gate! held)
                    (check-eq? 'succeeded (Analysis-Outcome-state (await-analysis fresh)))
                    (close! session path)
                    (barrier session))))

  (test-case "cache eviction leaves live documents usable and makes an evicted reopen cold"
    (define gates (make-async-channel))
    (define text "#lang racket/base\n(define value 1)\nvalue\n")
    (with-session (make-source-controller #:analyze (analyzer gates) #:resyntax #f #:closed-analysis-limit 1)
                  (lambda (session)
                    (define a (build-path scratch "eviction-a.rkt"))
                    (define b (build-path scratch "eviction-b.rkt"))
                    (define live (build-path scratch "eviction-live.rkt"))
                    (for ([path (in-list (list live a b))])
                      (open-analyzed! session gates path text)
                      (unless (equal? path live) (close! session path) (barrier session)))
                    (check-true (pair? (hash-ref (query session live 420 "textDocument/semanticTokens/full") 'data)))
                    (open! session a text)
                    (define cold (await gates))
                    (barrier session)
                    (check-false (available session a))
                    (release-gate! cold)
                    (await-analysis (acquire session a))
                    (open! session b text)
                    (define warm (await gates))
                    (barrier session)
                    (check-true (pair? (hash-ref (query session b 421 "textDocument/semanticTokens/full") 'data)))
                    (release-gate! warm)
                    (await-analysis (acquire session b))
                    (for ([path (in-list (list a b live))]) (close! session path))
                    (barrier session))))

  (for ([failure (in-list '(fail errors))])
    (test-case (format "refresh ~a cannot be retained as success" failure)
      (define gates (make-async-channel))
      (define text "#lang racket/base\n(define value 1)\nvalue\n")
      (with-session (make-source-controller #:analyze (analyzer gates) #:resyntax #f)
                    (lambda (session)
                      (define path (build-path scratch (format "refresh-~a.rkt" failure)))
                      (open-analyzed! session gates path text)
                      (close! session path)
                      (barrier session)
                      (open! session path text)
                      (define held (await gates))
                      (barrier session)
                      (define fresh (acquire session path))
                      (check-true (pair? (hash-ref (query session path 430 "textDocument/semanticTokens/full") 'data)))
                      (release-gate! held failure)
                      (check-eq? (Analysis-Outcome-state (await-analysis fresh))
                                 (if (eq? failure 'fail) 'failed 'errors))
                      (when (eq? failure 'errors)
                        (check-equal? (query session path 431 "textDocument/semanticTokens/full") (hasheq 'data '())))
                      (close! session path)
                      (barrier session)
                      (open! session path text)
                      (await gates)
                      (barrier session)
                      (check-false (available session path))
                      (close! session path)
                      (barrier session))))))

(define-runtime-path doc-location "../../doclib/doc.rkt")
(define doc-path (simplify-path doc-location #f))

(module+ test
  (test-case "doc-check-syntax definition jump survives closing doc.rkt while target refresh is held"
    (define gates (make-async-channel))
    (with-files
      (lambda (root)
        (define a (build-path root "jump.rkt"))
        (define source (format "#lang racket/base\n(require (file ~s))\ndoc-check-syntax\n"
                               (path->string doc-path)))
        (write-text a source)
        (with-session
          (make-source-controller
            #:resyntax #f
            #:analyze (lambda (snapshot)
                        (if (equal? (Source-Snapshot-id snapshot)
                                    (source-id (path->source doc-path)))
                            ((analyzer gates) snapshot)
                            (analyze-source snapshot))))
          (lambda (session)
            (open! session a source)
            (barrier session)
            (await-analysis (acquire session a))
            (open! session doc-path (file->string doc-path))
            (release-gate! (await gates))
            (barrier session)
            (define accepted (await-analysis (acquire session doc-path)))
            (check-eq? 'succeeded (Analysis-Outcome-state accepted))
            (define expected (query session a 440 "textDocument/definition" (Pos 2 0)))
            (check-equal? (hash-ref expected 'uri) (path->uri doc-path))
            (close! session doc-path)
            (barrier session)
            (request! session 441 "textDocument/definition" a
                      (hasheq 'position (->jsexpr (Pos 2 0))))
            (define held (await gates))
            (check-equal? (hash-ref (response session 441) 'result) expected)
            (check-false (sync/timeout 0 (acquire session doc-path)))
            (release-gate! held)
            (await-analysis (acquire session doc-path))
            (close! session a)
            (barrier session)))))))

(module+ test
  (test-case "unchanged Typed Racket source shows retained dependency type then fresh replacement"
    (define gates (make-async-channel))
    (with-files
      (lambda (root)
        (define dependency (build-path root "dep.rkt"))
        (define path (build-path root "uses-dep.rkt"))
        (define source (format "#lang typed/racket/base\n(require (file ~s))\n(define result value)\nresult\n"
                               (path->string dependency)))
        (write-text dependency "#lang typed/racket/base\n(provide value)\n(define value 1)\n")
        (write-text path source)
        (with-session (make-source-controller #:analyze (analyzer gates) #:resyntax #f)
                      (lambda (session)
                        (open-analyzed! session gates path source)
                        (define original (query session path 450 "textDocument/hover"))
                        (check-true (string-contains? (hash-ref original 'contents) "**Type**"))
                        (close! session path)
                        (barrier session)
                        (write-text dependency "#lang typed/racket/base\n(provide value)\n(define value \"new\")\n")
                        (open! session path source)
                        (define held (await gates))
                        (barrier session)
                        (define fresh (acquire session path))
                        (check-equal? (query session path 451 "textDocument/hover") original)
                        (check-false (sync/timeout 0 fresh))
                        (release-gate! held)
                        (check-eq? 'succeeded (Analysis-Outcome-state (await-analysis fresh)))
                        (define replacement (query session path 452 "textDocument/hover"))
                        (check-true (string-contains? (hash-ref replacement 'contents) "String"))
                        (check-not-equal? replacement original)
                        (close! session path)
                        (barrier session)))))))

(module+ test
  (test-case "closing a restored buffer during refresh retains only its available result"
    (define gates (make-async-channel))
    (define text "#lang racket/base\n(define value 1)\nvalue\n")
    (with-session (make-source-controller #:analyze (analyzer gates) #:resyntax #f)
                  (lambda (session)
                    (define path (build-path scratch "close-restored-refresh.rkt"))
                    (open-analyzed! session gates path text)
                    (define expected (query session path 460 "textDocument/semanticTokens/full"))
                    (for ([cycle (in-range 2)])
                      (close! session path)
                      (barrier session)
                      (open! session path text)
                      (await gates)
                      (barrier session)
                      (check-equal? (query session path 461 "textDocument/semanticTokens/full") expected))
                    (define pending (acquire session path))
                    (shutdown! session)
                    (check-eq? 'stopped (Analysis-Outcome-state (await pending))))))

  (test-case "restoration does not start Resyntax before fresh analysis succeeds"
    (define gates (make-async-channel))
    (define resyntax-starts (make-async-channel))
    (define old-enabled (get-resyntax-enabled))
    (define text "#lang racket/base\n(define value 1)\nvalue\n")
    (dynamic-wind
      (lambda () (set-resyntax-enabled! #t))
      (lambda ()
        (with-session
          (make-source-controller #:analyze (analyzer gates)
                                  #:resyntax (lambda (_text _uri) (async-channel-put resyntax-starts 'started) '()))
          (lambda (session)
            (define path (build-path scratch "restored-resyntax.rkt"))
            (open-analyzed! session gates path text)
            (await resyntax-starts)
            (close! session path)
            (barrier session)
            (open! session path text)
            (define held (await gates))
            (barrier session)
            (check-true (pair? (hash-ref (query session path 470 "textDocument/semanticTokens/full") 'data)))
            (check-false (sync/timeout 0 resyntax-starts))
            (release-gate! held)
            (await-analysis (acquire session path))
            (await resyntax-starts)
            (close! session path)
            (barrier session))))
      (lambda () (set-resyntax-enabled! old-enabled)))))

(module+ test
  (test-case "closed-file acquisition refreshes LRU recency while fresh analysis is held"
    (define gates (make-async-channel))
    (define text "#lang racket/base\n(define value 1)\nvalue\n")
    (with-files
      (lambda (root)
        (define a (build-path root "lru-a.rkt"))
        (define b (build-path root "lru-b.rkt"))
        (define c (build-path root "lru-c.rkt"))
        (for ([path (in-list (list a b c))])
          (write-text path text))
        (with-session
          (make-source-controller #:analyze (analyzer gates) #:resyntax #f #:closed-analysis-limit 2)
          (lambda (session)
            (for ([path (in-list (list a b))])
              (open-analyzed! session gates path text)
              (close! session path)
              (barrier session))
            ;; Reading A's retained facts must protect it from eviction by C.
            ;; Hold A's refresh so a replacement cannot supply that protection.
            (check-pred Analysis-Summary? (available session a))
            (define held-a (await gates))
            (open-analyzed! session gates c text)
            (close! session c)
            (barrier session)
            (check-pred Analysis-Summary? (available session a))
            (check-false (available session b))
            (define held-b (await gates))
            (release-gate! held-a)
            (release-gate! held-b)
            (check-eq? 'succeeded (Analysis-Outcome-state (await-analysis (acquire session a))))
            (check-eq? 'succeeded (Analysis-Outcome-state (await-analysis (acquire session b))))))))))
