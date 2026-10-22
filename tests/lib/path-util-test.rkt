#lang racket/base

(require rackunit
         racket/file
         racket/list
         racket/path
         "../../common/path-util.rkt")

(module+ test
  (test-case "file URI source paths normalize dot segments and escaped names"
    (define root (uri->path (path->uri (current-directory))))
    (for ([path (in-list
                  (list root
                        (build-path root "plain.rkt")
                        (build-path root 'same "dot.rkt")
                        (build-path root "child" 'up "parent.rkt")
                        (build-path root 'same "space #%; λ.rkt")
                        (build-path root 'same)
                        (build-path root "child" 'up)
                        (uri->path (path->uri (car (explode-path root))))))])
      (check-equal? (normalize-source-path (uri->path (path->uri path)))
                    (normalize-source-path path))))

  (test-case "path-only URIs use standard escaping after lexical normalization"
    (define root (current-directory))
    (check-equal? (path->uri (build-path root 'same "dot.rkt"))
                  (path->uri (build-path root "dot.rkt")))
    (check-equal? (path->uri (build-path root "child" 'up "parent.rkt"))
                  (path->uri (build-path root "parent.rkt")))
    (check-regexp-match #rx"space%20%23%25" (path->uri (build-path root "space #%.rkt"))))

  (test-case "dot segments through a symlink stay in the lexical parent"
    (define root (make-temporary-file "path-util~a" 'directory))
    (dynamic-wind
      void
      (lambda ()
        (define nest (build-path root "nest"))
        (make-directory* (build-path nest "real"))
        (make-file-or-directory-link (build-path "nest" "real")
                                     (build-path root "out-link"))
        (define crossed (build-path root "out-link" 'up "file.rkt"))
        (check-equal? (normalize-source-path crossed)
                      (normalize-source-path (build-path root "file.rkt")))
        (check-true (directory-contains? root crossed))
        (check-true (directory-contains? nest (build-path nest "file.rkt")))
        (check-false (directory-contains? nest crossed)))
      (lambda () (delete-directory/files root)))))

(module+ test
  (test-case "suffix fallback reads URI paths without treating virtual URIs as files"
    (check-equal? (uri->suffix "untitled:buffer.scrbl") "scrbl")
    (check-equal? (uri->suffix "git://remote/main.rhm?rev=.scrbl#cell.rktd") "rhm")
    (for ([uri (in-list '("untitled:Untitled-1" "untitled:" "file:///" "file:///tmp/"
                          "file:///tmp/." "file:///tmp/.."))])
      (check-false (uri->suffix uri)))))
