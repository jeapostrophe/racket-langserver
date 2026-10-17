#lang racket/base

(require rackunit
         racket/list
         racket/path
         "../../common/path-util.rkt")

(module+ test
  (test-case "file URI source paths preserve exact segments and escaped names"
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
      (check-equal? (uri->path (path->uri path)) path)))

  (test-case "dot segments remain visible in file URIs"
    (define root (current-directory))
    (check-regexp-match #rx"/[.]/[^/]+$" (path->uri (build-path root 'same "dot.rkt")))
    (check-regexp-match #rx"/child/[.][.]/[^/]+$"
                        (path->uri (build-path root "child" 'up "parent.rkt")))))

(module+ test
  (test-case "suffix fallback reads URI paths without treating virtual URIs as files"
    (check-equal? (uri->suffix "untitled:buffer.scrbl") "scrbl")
    (check-equal? (uri->suffix "git://remote/main.rhm?rev=.scrbl#cell.rktd") "rhm")
    (for ([uri (in-list '("untitled:Untitled-1" "untitled:" "file:///" "file:///tmp/"
                          "file:///tmp/." "file:///tmp/.."))])
      (check-false (uri->suffix uri)))))
