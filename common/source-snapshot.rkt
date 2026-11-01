#lang racket/base

;; Module loading for one analysis that reads open editor buffers instead of
;; disk files. Project dependencies are read from source so compiled
;; intermediates cannot retain an older macro expansion. Libraries outside the
;; project keep their ordinary compiled loading policy.
(require racket/list
         racket/path
         setup/dirs
         syntax/modcode)

(provide call-with-source-snapshot)

;; sources: complete normalized path -> buffer text, or #f when two buffers
;;   claim the same path.
;; source?: whether a file outside `sources` loads from source.
;; on-load: receives each module file the expansion loads.
;; Installed collections keep compiled loading even below a project folder,
;; for example when a workspace folder contains the Racket installation.
(define (installed-library? file)
  (define parts (explode-path file))
  (for/or ([dir (in-list (filter values (list* (find-collects-dir) (find-pkgs-dir)
                                               (find-user-collects-dir) (find-user-pkgs-dir)
                                               (current-library-collection-paths))))])
    (define root (explode-path (simplify-path (path->complete-path dir) #f)))
    (and (<= (length root) (length parts)) (equal? root (take parts (length root))))))

(define (call-with-source-snapshot sources proc
                                   #:source? [source? (lambda (_) #f)]
                                   #:on-load [on-load void])
  (define ordinary-load (current-load/use-compiled))
  (define ordinary-open (moddep-current-open-input-file))
  (define (load-snapshot file expected)
    (define requested (simple-form-path file))
    (define legacy (path-replace-extension requested #".ss"))
    (define full
      (if (and expected (equal? (path-get-extension requested) #".rkt")
               (not (hash-has-key? sources requested)) (not (file-exists? requested))
               (or (hash-has-key? sources legacy) (file-exists? legacy)))
          legacy requested))
    (when expected (on-load full))
    (cond
      [(and expected (or (hash-has-key? sources full)
                         (and (file-exists? full) (source? full) (not (installed-library? full)))))
       ;; A #f-headed request permits independent compiled loading only. Let
       ;; the resolver request the enclosing source module instead.
       (unless (and (pair? expected) (not (car expected)))
         (parameterize ([current-load-relative-directory (path-only full)]
                        [current-module-declare-source full])
           (eval (get-module-code full #:rkt-try-ss? #f
                                  #:choose (lambda _ 'src)))))]
      [else (ordinary-load file expected)]))
  (parameterize ([current-load/use-compiled load-snapshot]
                 [moddep-current-open-input-file
                  (lambda (file)
                    (define full (simple-form-path file))
                    (cond
                      [(hash-has-key? sources full)
                       (define text (hash-ref sources full))
                       (unless text (error 'source-snapshot "conflicting buffers for ~a" full))
                       (open-input-string text full)]
                      [else (ordinary-open file)]))])
    (proc)))
