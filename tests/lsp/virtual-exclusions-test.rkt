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

(define (initialize! session options)
  (async-channel-put (Session-clients session)
                     (hasheq 'id 0 'method "initialize"
                             'params (hasheq 'processId (json-null) 'capabilities (hasheq)
                                             'rootUri (json-null) 'rootPath (json-null)
                                             'initializationOptions options)))
  (check-true (hash-has-key? (response session 0) 'result)))

(define (open-uri! session uri)
  (notify session "textDocument/didOpen"
          (hasheq 'textDocument (hasheq 'uri uri 'languageId "racket" 'version 0 'text text))))

(define methods
  '("hover" "codeAction" "completion" "signatureHelp" "definition"
    "documentHighlight" "references" "documentSymbol" "inlayHint" "rename"
    "prepareRename" "formatting" "rangeFormatting" "onTypeFormatting"
    "semanticTokens/full" "semanticTokens/range"))

(module+ test
  (test-case "startup exclusions ignore lifecycle notifications and define every feature response"
    (define analyses (make-async-channel))
    (with-session
      (make-source-controller
        #:resyntax #f
        #:analyze (lambda (snapshot) (async-channel-put analyses snapshot) (analyze-source snapshot)))
      (lambda (session)
        (define exact "untitled:Skip#one")
        (define ignored-file "file:///excluded.rkt")
        (initialize! session (hasheq 'ignoredUriSchemes '("GIT") 'ignoredUris (list exact ignored-file)))
        (for ([uri (in-list (list "git:/project/main.rkt?rev=one#cell" exact ignored-file))])
          (notify session "textDocument/didChange"
                  (hasheq 'textDocument (hasheq 'uri uri 'version 1)
                          'contentChanges (list (hasheq 'text text))))
          (open-uri! session uri)
          (notify session "textDocument/didClose" (hasheq 'textDocument (hasheq 'uri uri))))
        (barrier session)
        (check-false (sync/timeout 0 analyses))
        (check-false (sync/timeout 0 (Session-notifications session)))
        (check-false (owner session (lambda (_) (lsp-get-doc exact #f))))
        (for ([method (in-list methods)] [id (in-naturals 1)])
          (async-channel-put (Session-clients session)
                             (hasheq 'id id 'method (string-append "textDocument/" method)
                                     'params (hasheq 'textDocument (hasheq 'uri exact))))
          (check-equal? (hash-ref (hash-ref (response session id) 'error) 'code)
                        (->jsexpr ErrorCode-RequestFailed)))
        ;; Exact exclusions retain query/fragment distinctions.
        (define allowed "untitled:Skip#two")
        (open-uri! session allowed)
        (barrier session)
        (await analyses)
        (check-not-false (owner session (lambda (_) (lsp-get-doc allowed #f))))
        ;; Live configuration cannot change startup routing in either direction.
        (notify session "workspace/didChangeConfiguration"
                (hasheq 'settings
                        (hasheq 'racket-langserver
                                (hasheq 'ignoredUriSchemes '() 'ignoredUris (list allowed)))))
        (open-uri! session exact)
        (barrier session)
        (check-false (owner session (lambda (_) (lsp-get-doc exact #f))))
        (check-not-false (owner session (lambda (_) (lsp-get-doc allowed #f))))
        (notify session "textDocument/didClose" (hasheq 'textDocument (hasheq 'uri allowed)))
        (barrier session))))

  (test-case "empty defaults in another session analyze previously excluded URIs"
    (define analyses (make-async-channel))
    (with-session
      (make-source-controller #:resyntax #f
                              #:analyze (lambda (snapshot)
                                          (async-channel-put analyses snapshot)
                                          (analyze-source snapshot)))
      (lambda (session)
        (initialize! session (hasheq))
        (define uri "git:/project/main.rkt?rev=one#cell")
        (open-uri! session uri)
        (barrier session)
        (await analyses)
        (check-not-false (owner session (lambda (_) (lsp-get-doc uri #f))))
        (notify session "textDocument/didClose" (hasheq 'textDocument (hasheq 'uri uri)))
        (barrier session))))

  (test-case "closed virtual feature requests receive unavailable without acquiring a source"
    (with-session (make-source-controller #:resyntax #f)
                  (lambda (session)
                    (async-channel-put (Session-clients session)
                                       (hasheq 'id 1 'method "textDocument/hover"
                                               'params (hasheq 'textDocument (hasheq 'uri "untitled:Closed")
                                                               'position (hasheq 'line 0 'character 0))))
                    (check-equal? (hash-ref (hash-ref (response session 1) 'error) 'code)
                                  (->jsexpr ErrorCode-RequestFailed))))))
