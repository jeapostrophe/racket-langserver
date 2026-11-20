#lang racket/base

(module+ test
  (require "controller-support.rkt"
           "../json-dsl.rkt"
           json
           rackunit
           racket/async-channel
           racket/path
           "../../common/interfaces.rkt"
           "../../common/json-util.rkt"
           "../../common/path-util.rkt"
           "../../common/source.rkt"
           "../../doclib/internal-types.rkt"
           "../../lsp/analysis.rkt"
           "../../lsp/source-controller.rkt"
           "../../workspace/api.rkt"
           "../../workspace/current.rkt"
           "../../workspace/source-store.rkt")

  (define (module-text name)
    (format "#lang racket/base\n(require racket/list)\n(define (~a unused) (first '(1)))\n(~a 2)\n"
            name name))

  (define (analyzed session uri)
    (define event (acquire session (uri->source uri)))
    (check-eq? 'succeeded (Analysis-Outcome-state (await event))))

  (define (feature session id method uri [extra (hasheq)])
    (request! session id method uri extra)
    (response session id))

  (define (check-local-results session uri)
    (define pos (->jsexpr (Pos 3 1)))
    (define definition (feature session 1 "textDocument/definition" uri (json {#:position pos})))
    (check-equal? (Location-uri (jsexpr-> Location (hash-ref definition 'result))) uri)
    (define references
      (feature session 2 "textDocument/references" uri
               (json {#:position pos #:context {#:includeDeclaration #t}})))
    (define ref-locs (jsexpr-> (listof Location) (hash-ref references 'result)))
    (check-equal? (length ref-locs) 2)
    (for ([location (in-list ref-locs)])
      (check-equal? (Location-uri location) uri))
    (define rename
      (feature session 3 "textDocument/rename" uri (json {#:position pos #:newName "renamed"})))
    (check-equal? (hash-keys (WorkspaceEdit-changes (jsexpr-> WorkspaceEdit (hash-ref rename 'result))))
                  (list (string->symbol uri)))
    (define actions
      (feature session 4 "textDocument/codeAction" uri
               (json {#:range (->jsexpr (Range (Pos 2 0) (Pos 2 40)))
                      #:context {#:diagnostics []}})))
    (define action-list (jsexpr-> (listof CodeAction) (hash-ref actions 'result)))
    (check-not-equal? action-list '())
    (for ([action (in-list action-list)])
      (check-equal? (hash-keys (WorkspaceEdit-changes (CodeAction-edit action)))
                    (list (string->symbol uri))))
    (define imported-references
      (feature session 6 "textDocument/references" uri
               (json {#:position (->jsexpr (Pos 2 25))
                      #:context {#:includeDeclaration #t}})))
    (define imported-locs (jsexpr-> (listof Location) (hash-ref imported-references 'result)))
    (check-equal? (length imported-locs) 1)
    (check-equal? (Location-uri (car imported-locs)) uri)
    (check-equal? (hash-ref (feature session 5 "textDocument/definition" uri
                                     (json {#:position (->jsexpr (Pos 2 25))}))
                            'result)
                  (json null)))

  (test-case "live file and matching virtual paths keep private analysis, locations and edits"
    (define root (car (explode-path scratch)))
    (workspace-add-folder! current-workspace root)
    (dynamic-wind
      void
      (lambda ()
        (with-session
          (make-source-controller #:read (lambda (_) (error 'disk "unexpected disk acquisition"))
                                  #:resyntax #f)
          (lambda (session)
            (define file-path (build-path scratch "virtual-coexist.rkt"))
            (define file-uri (path->uri file-path))
            (define virtual-prefix (regexp-replace #rx"^file:" file-uri "git:"))
            (define a (string-append virtual-prefix "?rev=one#cell"))
            (define b (string-append virtual-prefix "?rev=two#cell"))
            (for ([uri (in-list (list file-uri a b))]
                  [name (in-list '(live alpha beta))])
              (open! session uri (module-text name)))
            (barrier session)
            (for ([uri (in-list (list file-uri a b))])
              (analyzed session uri))
            (notify session "workspace/didChangeWorkspaceFolders"
                    (hasheq 'event
                            (hasheq 'added (list (hasheq 'uri (path->uri root) 'name "root"))
                                    'removed '())))
            (barrier session)
            (define live-binding (Module-Binding file-path '() 0 'live))
            (check-not-false (workspace-definition-location current-workspace live-binding))
            (for ([uri (in-list (list a b))]
                  [name (in-list '(alpha beta))])
              (check-false
                (workspace-definition-location current-workspace
                                               (Module-Binding file-path '() 0 name)))
              (check-local-results session uri))
            (define publications
              (for/list ([_ (in-range 3)])
                (await (Session-notifications session))))
            (check-equal? (sort (map (lambda (msg) (hash-ref (hash-ref msg 'params) 'uri)) publications)
                                string<?)
                          (sort (list file-uri a b) string<?))
            (for ([msg (in-list publications)])
              (define uri (hash-ref (hash-ref msg 'params) 'uri))
              (define unused
                (filter (lambda (diag) (equal? (hash-ref diag 'message) "unused variable"))
                        (hash-ref (hash-ref msg 'params) 'diagnostics)))
              (check-equal? (length unused) 1)
              (check-equal? (hash-ref (car unused) 'source) uri))
            (close! session a)
            (barrier session)
            (check-equal? (hash-ref (await (Session-notifications session)) 'params)
                          (hasheq 'uri a 'diagnostics '()))
            (check-false (view session (uri->source a)))
            (check-not-false (view session (uri->source b)))
            (check-not-false (workspace-definition-location current-workspace live-binding))
            (check-local-results session b)
            (for ([uri (in-list (list file-uri b))])
              (close! session uri))
            (barrier session))))
      (lambda () (workspace-remove-folder! current-workspace root))))

  (test-case "file and workspace events preserve virtual revisions and cached analysis"
    (define root (car (explode-path scratch)))
    (workspace-add-folder! current-workspace root)
    (dynamic-wind
      void
      (lambda ()
        (define analyses (make-async-channel))
        (with-session
          (make-source-controller
            #:read (lambda (_) (error 'disk "events must not acquire disk"))
            #:resyntax #f
            #:analyze (lambda (snapshot)
                        (async-channel-put analyses (Source-Snapshot-id snapshot))
                        (analyze-source snapshot)))
          (lambda (session)
            (define path (build-path scratch "event-coexist.rkt"))
            (define file-uri (path->uri path))
            (define virtual-uri (string-append (regexp-replace #rx"^file:" file-uri "notebook:") "#cell"))
            (define virtual-source (uri->source virtual-uri))
            (open! session file-uri (module-text 'live))
            (open! session virtual-uri (module-text 'alpha))
            (barrier session)
            (analyzed session file-uri)
            (analyzed session virtual-uri)
            (await analyses)
            (await analyses)
            (define before (view session virtual-source))
            (define (unchanged)
              (barrier session)
              (check-eq? before (view session virtual-source)))
            (for ([type (in-list (list FileChangeType-created
                                       FileChangeType-changed
                                       FileChangeType-deleted))])
              (notify session "workspace/didChangeWatchedFiles"
                      (->jsexpr (DidChangeWatchedFilesParams (list (FileEvent file-uri type)))))
              (unchanged))
            (for ([add? (in-list '(#f #t))])
              (define folders (list (WorkspaceFolder (path->uri root) "root")))
              (notify session "workspace/didChangeWorkspaceFolders"
                      (->jsexpr
                        (DidChangeWorkspaceFoldersParams
                          (WorkspaceFoldersChangeEvent (if add? folders '())
                                                       (if add? '() folders)))))
              (unchanged))
            (check-false (sync/timeout 0 analyses))
            (define next-uri (path->uri (build-path scratch "event-renamed.rkt")))
            (notify session "workspace/didRenameFiles"
                    (->jsexpr (RenameFilesParams (list (FileRename file-uri next-uri)))))
            (unchanged)
            (analyzed session file-uri)
            (check-equal? (await analyses) (source-id (path->source path))
                          "rename reanalyzes only the open file")
            (notify session "workspace/didRenameFiles"
                    (->jsexpr (RenameFilesParams (list (FileRename virtual-uri "untitled:renamed")))))
            (notify session "workspace/didChangeWorkspaceFolders"
                    (->jsexpr
                      (DidChangeWorkspaceFoldersParams
                        (WorkspaceFoldersChangeEvent
                          (list (WorkspaceFolder "untitled:folder" "ignored"))
                          '()))))
            (unchanged)
            (check-false (sync/timeout 0 analyses))
            (check-equal? (Source-Snapshot-text
                            (Source-Result-snapshot (observation session virtual-source)))
                          (module-text 'alpha))
            (check-local-results session virtual-uri)
            (for ([uri (in-list (list file-uri virtual-uri))])
              (close! session uri))
            (barrier session))))
      (lambda () (workspace-remove-folder! current-workspace root)))))
