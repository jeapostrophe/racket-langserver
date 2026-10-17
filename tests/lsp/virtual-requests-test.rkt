#lang racket/base

(require json
         rackunit
         racket/async-channel
         "../../common/interfaces.rkt"
         "../../common/json-util.rkt"
         "../../lsp/analysis.rkt"
         "../../lsp/lsp.rkt"
         "../../lsp/source-controller.rkt"
         (submod "source-controller-test.rkt" test-support))

(define text "#lang racket/base\n(define value 1)\nvalue\n")

(define (initialize! session)
  (async-channel-put (Session-clients session)
                     (hasheq 'id 0 'method "initialize"
                             'params (hasheq 'processId (json-null) 'capabilities (hasheq)
                                             'rootUri (json-null) 'rootPath (json-null)
                                             'initializationOptions (hasheq))))
  (check-true (hash-has-key? (response session 0) 'result)))

(define (open-uri! session uri)
  (notify session "textDocument/didOpen"
          (hasheq 'textDocument (hasheq 'uri uri 'languageId "racket" 'version 0 'text text))))

(module+ test
  (test-case "Git virtual documents are analyzed"
    (define analyses (make-async-channel))
    (with-session
      (make-source-controller #:resyntax #f
                              #:analyze (lambda (snapshot)
                                          (async-channel-put analyses snapshot)
                                          (analyze-source snapshot)))
      (lambda (session)
        (initialize! session)
        (define uri "git:/project/main.rkt?rev=one#cell")
        (open-uri! session uri)
        (barrier session)
        (await analyses)
        (check-not-false (owner session (lambda (_) (lsp-get-doc uri #f))))
        (notify session "textDocument/didClose" (hasheq 'textDocument (hasheq 'uri uri)))
        (barrier session))))

  (test-case "closed virtual feature requests receive RequestFailed"
    (with-session (make-source-controller #:resyntax #f)
                  (lambda (session)
                    (async-channel-put (Session-clients session)
                                       (hasheq 'id 1 'method "textDocument/hover"
                                               'params (hasheq 'textDocument (hasheq 'uri "untitled:Closed")
                                                               'position (hasheq 'line 0 'character 0))))
                    (check-equal? (hash-ref (hash-ref (response session 1) 'error) 'code)
                                  (->jsexpr ErrorCode-RequestFailed))))))
