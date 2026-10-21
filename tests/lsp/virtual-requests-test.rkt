#lang racket/base

(require rackunit
         racket/async-channel
         "../../common/interfaces.rkt"
         "../../common/json-util.rkt"
         "../../lsp/source-controller.rkt"
         (submod "source-controller-test.rkt" test-support))

(module+ test
  (test-case "closed virtual feature requests receive RequestFailed"
    (with-session (make-source-controller #:resyntax #f)
                  (lambda (session)
                    (async-channel-put (Session-clients session)
                                       (hasheq 'id 1 'method "textDocument/hover"
                                               'params (hasheq 'textDocument (hasheq 'uri "untitled:Closed")
                                                               'position (hasheq 'line 0 'character 0))))
                    (check-equal? (hash-ref (hash-ref (response session 1) 'error) 'code)
                                  (->jsexpr ErrorCode-RequestFailed))))))
