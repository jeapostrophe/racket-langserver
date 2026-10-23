#lang racket/base

(require rackunit
         racket/list
         "../../common/path-util.rkt"
         "../../common/source.rkt")

(module+ test
  (test-case "file identity is independent of the original URI spelling"
    (define uri "file:///project/child/.././%6dain.rkt")
    (define source (uri->source uri))
    (define canonical (uri->source "file:///project/main.rkt"))
    (check-true (Source? source))
    (check-true (Source-Id? (source-id source)))
    (check-equal? (source-id source) (source-id canonical))
    (check-not-equal? source canonical)
    (check-equal? (source-uri source) uri)
    (check-equal? (source-file-path source) (normalize-source-path (uri->path uri)))
    (check-equal? (source-syntax-source source) (normalize-source-path (uri->path uri)))
    (check-equal? (hash-ref (hash (source-id source) 'found) (source-id canonical)) 'found))

  (test-case "virtual identities retain the complete URI and remain distinct from file identities"
    (define uris '("untitled:Untitled-1"
                   "git:/project/main.rkt?rev=one#cell"
                   "git:/project/main.rkt?rev=two#cell"
                   "git:/project/main.rkt?rev=one#other"
                   "git://remote/project/main.rkt?rev=one#cell"
                   "notebook:/project/main.rkt"))
    (define sources (map uri->source uris))
    (check-equal? (map source-uri sources) uris)
    (check-equal? (length (remove-duplicates (map source-id sources))) (length uris))
    (for ([source (in-list sources)] [uri (in-list uris)])
      (check-true (Source-Id? (source-id source)))
      (check-false (source-file-path source))
      (check-equal? (source-syntax-source source) uri)
      (check-equal? (source-location-uri source uri) uri))
    (check-not-equal? (source-id (uri->source "file:///project/main.rkt"))
                      (source-id (uri->source "git:/project/main.rkt"))))

  (test-case "relative paths complete at the caller current directory"
    (define path (build-path 'same "child" 'up "main.rkt"))
    (define source (path->source path))
    (check-equal? (source-file-path source) (normalize-source-path path))
    (check-equal? (source-syntax-source source) (normalize-source-path path))
    (check-equal? (source-id source) (source-id (path->source path)))
    (check-equal? (source-id source)
                  (source-id (path->source (path->complete-path path))))
    (check-equal? (source-uri source) (path->uri path))
    (check-equal? (source-location-uri source path) (source-uri source)))

  (test-case "file descriptors collapse dot aliases"
    (define root (current-directory))
    (for ([path (in-list (list (build-path root 'same "main.rkt")
                               (build-path root "child" 'up "main.rkt")))])
      (define source (path->source path))
      (check-equal? (source-file-path source) (normalize-source-path path))
      (check-equal? (source-syntax-source source) (normalize-source-path path))
      (check-equal? (source-id source) (source-id (uri->source (path->uri path))))
      (check-equal? (source-id source)
                    (source-id (path->source (build-path root "main.rkt"))))))

  (test-case "same-source syntax locations retain aliases and external paths use their URI"
    (define uri "file:///project/child/.././%6dain.rkt")
    (define source (uri->source uri))
    (check-equal? (source-location-uri source (uri->path uri)) uri)
    (define external (uri->path "file:///project/other.rkt"))
    (check-equal? (source-location-uri source external) (path->uri external))
    (define virtual (uri->source "notebook:/project/main.rkt#cell"))
    (check-equal? (source-location-uri virtual (source-syntax-source virtual))
                  (source-uri virtual))
    (check-equal? (source-location-uri virtual external) (path->uri external)))

  (test-case "mutable URI inputs cannot change descriptor addresses or identities"
    (for ([original (in-list '("file:///project/%6dain.rkt"
                               "notebook:/project/main.rkt?rev=one#cell"))])
      (define input (string-copy original))
      (define source (uri->source input))
      (define id (source-id source))
      (define table (hash id 'present))
      (string-set! input 0 #\x)
      (check-equal? (source-uri source) original)
      (check-true (immutable? (source-uri source)))
      (check-equal? (source-id source) id)
      (check-equal? (hash-ref table (source-id (uri->source original))) 'present)
      (when (string? (source-syntax-source source))
        (check-true (immutable? (source-syntax-source source)))))))
