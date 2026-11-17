#lang racket/base

(module+ test
  (require compiler/module-suffix
           rackunit
           "../../common/interfaces.rkt"
           "../../common/json-util.rkt"
           "../json-dsl.rkt"
           "../stdio-server.rkt")

  (define expected-capabilities
    (json
      {#:codeActionProvider #t
       #:completionProvider {#:triggerCharacters ["("]}
       #:definitionProvider #t
       #:documentFormattingProvider #t
       #:documentHighlightProvider #t
       #:documentOnTypeFormattingProvider {#:firstTriggerCharacter ")"
                                           #:moreTriggerCharacter ["\n" "]"]}
       #:documentRangeFormattingProvider #t
       #:documentSymbolProvider #t
       #:hoverProvider #t
       #:inlayHintProvider #t
       #:referencesProvider #t
       #:renameProvider #t
       #:semanticTokensProvider
       {#:full #t
        #:range #t
        #:legend {#:tokenModifiers (map ->jsexpr *semantic-token-modifiers*)
                  #:tokenTypes (map ->jsexpr *semantic-token-types*)}}
       #:signatureHelpProvider {#:triggerCharacters [" " ")" "]"]}
       #:textDocumentSync {#:change (->jsexpr TextDocumentSyncKind-Incremental)
                           #:openClose #t
                           #:willSave #f
                           #:willSaveWaitUntil #f}
       #:workspace
       {#:workspaceFolders {#:changeNotifications #t}
        #:fileOperations
        {#:didRename
         {#:filters
          (for/list ([ext (in-list (get-module-suffixes))])
            (json {#:scheme "file"
                   #:pattern {#:glob (format "**/*.~a" ext)}}))}}}}))

  (test-case "stdio server initializes with advertised capabilities and shuts down cleanly"
    (call-with-stdio-client
      (lambda (client)
        (define init-result (stdio-initialize! client 0))
        (check-equal? init-result (json {#:capabilities expected-capabilities}))
        (define-values (shutdown-res exit-code) (stdio-shutdown-and-exit! client 1))
        (check-equal? shutdown-res (json null))
        (check-equal? exit-code 0)
        (check-equal? (get-output-string (Stdio-Client-stderr-output client)) "")))))
