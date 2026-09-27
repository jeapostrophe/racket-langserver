#lang racket/base

(require rackunit
         "../../doclib/doc.rkt"
         "../../lsp/lsp.rkt"
         "../../lsp/safedoc.rkt"
         "../../lsp/text-document.rkt"
         "../../lsp/workspace.rkt")

(define uri "file:///watched-create.rkt")
(define source-text "#lang racket/base\n(define other 0)\n(define private 6)\n")

(define (notify-created!)
  (didChangeWatchedFiles
    (hasheq 'changes (list (hasheq 'uri uri 'type 1)))))

(define (with-open-document proc)
  (dynamic-wind
    (lambda ()
      (did-open! void void
                 (hasheq 'textDocument
                         (hasheq 'uri uri 'languageId "racket" 'version 7 'text source-text))))
    proc
    (lambda () (lsp-close-doc! uri))))

(module+ test
  (test-case
    "a delayed watched create preserves the client-open document and version"
    (with-open-document
      (lambda ()
        (define opened (lsp-get-doc uri))
        (notify-created!)
        (check-eq? (lsp-get-doc uri) opened)
        (with-read-doc (lsp-get-doc uri)
          (lambda (doc)
            (check-equal? (doc-get-text doc) source-text)
            (check-equal? (Doc-version doc) 7))))))

  (test-case
    "incremental edits still apply after a delayed watched create"
    (with-open-document
      (lambda ()
        (notify-created!)
        (check-not-exn
          (lambda ()
            (did-change! void
                         (hasheq 'textDocument (hasheq 'uri uri 'version 8)
                                 'contentChanges
                                 (list (hasheq 'range
                                               (hasheq 'start (hasheq 'line 2 'character 0)
                                                       'end (hasheq 'line 2 'character 18))
                                               'text "(define private 9)"))))))
        (with-read-doc (lsp-get-doc uri)
          (lambda (doc)
            (check-equal? (doc-get-text doc)
                          "#lang racket/base\n(define other 0)\n(define private 9)\n")
            (check-equal? (Doc-version doc) 8)))))))
