#lang racket

(module+ test
  (require rackunit
           "../../doc.rkt"
           "../../internal-types.rkt"
           "../../interfaces.rkt"
           "../../private/workspace.rkt"
           "../../service/workspace-references.rkt"
           "../../path-util.rkt"
           racket/file)

  (test-case
    "doc-references includes cross-file workspace bindings"
    ;; Setup: create a temp directory with two files:
    ;;   lib.rkt: (provide foo) (define (foo) 42)
    ;;   client.rkt: (require "lib.rkt") (foo)
    ;; Expanding client.rkt should record a reference from client -> lib's foo.
    ;; Then doc-references on lib.rkt for foo should include the client location.
    (define tmp-dir (normalize-path "./.tmp"))
    (make-directory* tmp-dir)

    (define lib-path (build-path tmp-dir "lib.rkt"))
    (define lib-text "#lang racket/base\n(provide foo)\n(define (foo) 42)\n")
    (with-output-to-file
      lib-path
      (lambda () (display lib-text))
      #:exists 'replace)
    (define client-path (build-path tmp-dir "client.rkt"))
    (define client-text "#lang racket/base\n(require \"lib.rkt\")\n(foo)\n")
    (with-output-to-file
      client-path
      (lambda () (display client-text))
      #:exists 'replace)

    ;; Register workspace folder and reset state
    (reset-workspace-references!)
    (add-workspace-folder! tmp-dir)

    (define lib-uri (path->uri lib-path))
    (define client-uri (path->uri client-path))

    ;; Expand both files so syncheck annotations fire
    (define lib-doc (make-doc lib-uri lib-text))
    (check-true (doc-expand! lib-doc) "lib.rkt should expand successfully")

    (define client-doc (make-doc client-uri client-text))
    (check-true (doc-expand! client-doc) "client.rkt should expand successfully")

    ;; Check workspace bindings for foo in lib
    ;; client.rkt line 2: "(foo)" — foo is at char 1..4
    (check-equal? (find-workspace-bindings lib-uri 'foo)
                  (list (Location #:uri client-uri
                                  #:range (Range (Pos 2 1) (Pos 2 4)))))

    ;; doc-references on lib.rkt at foo's definition site (line 2, char 9)
    ;; Should include:
    ;;   1. local: foo in (provide foo) at line 1, char 9..12
    ;;   2. workspace: foo in client.rkt at line 2, char 1..4
    (define refs (doc-references lib-doc lib-uri (Pos 2 9) #t))
    (check-equal? (length refs) 2)
    (check-equal? refs
                  (list
                    (Location #:uri lib-uri
                              #:range (Range (Pos 1 9) (Pos 1 12)))
                    (Location #:uri client-uri
                              #:range (Range (Pos 2 1) (Pos 2 4)))))

    ;; Cleanup
    (reset-workspace-references!)
    (delete-file lib-path)
    (delete-file client-path)
    (delete-directory tmp-dir))
  )
