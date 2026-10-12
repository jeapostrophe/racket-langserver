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
  (test-case "virtual identity preserves complete URIs and separates file identities"
    (define uris '("untitled:Untitled-1"
                   "git:/project/main.rkt?rev=one#cell"
                   "git:/project/main.rkt?rev=two#cell"
                   "git:/project/main.rkt?rev=one#other"
                   "git://remote/project/main.rkt?rev=one#cell"
                   "notebook:/project/main.rkt"))
    (define sources (map make-source uris))
    (check-equal? (map source-uri sources) uris)
    (check-equal? (map source-id sources) (map Virtual-Id uris))
    (check-equal? (length (remove-duplicates (map source-id sources))) (length uris))
    (for ([source (in-list sources)])
      (check-true (complete-path? (source-backend-path source)))
      (check-false (file-exists? (source-backend-path source)))
      (check-equal? (source-location->uri source (source-backend-path source))
                    (source-uri source)))
    (check-not-equal? (source-id (make-source "file:///project/main.rkt"))
                      (source-id (make-source "git:/project/main.rkt")))
    (check-not-equal? (source-backend-path (make-source (car uris)))
                      (source-backend-path (car sources))))

  (test-case "suffix fallback reads URI paths without treating virtual URIs as files"
    (check-equal? (uri->suffix "untitled:buffer.scrbl") "scrbl")
    (check-equal? (uri->suffix "git://remote/main.rhm?rev=.scrbl#cell.rktd") "rhm")
    (check-false (uri->suffix "untitled:Untitled-1"))))
