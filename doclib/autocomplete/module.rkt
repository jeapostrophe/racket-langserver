;; copied from https://github.com/yjqww6/drcomplete/blob/master/drcomplete-module/private/main.rkt

; drcomplete
; Copyright (c) 2019 yjqww6

; This package is distributed under the GNU Lesser General Public
; License (LGPL).  This means that you can link drcomplete into proprietary
; applications, provided you follow the rules stated in the LGPL.  You
; can also modify this package; if you distribute a modified version,
; you must distribute it under the terms of the LGPL, which in
; particular means that you must release the source code for the
; modified software.  See http://www.gnu.org/copyleft/lesser.html
; for more information.
#lang racket
(require setup/link)

(define (collections)
  (define h1
    (for/fold ([h (hash)])
              ([p (in-list (append
                             (current-library-collection-paths)
                             (links #:root? #t #:user? #f)
                             (links #:root? #t #:user? #t)))]
               #:when (directory-exists? p)
               [d (in-list (directory-list p #:build? #t))]
               #:when (directory-exists? d))
      (define-values (b f s) (split-path d))
      (define k (path->string f))
      (define v (path->string d))
      (hash-update h k (λ (s) (set-add s v)) (λ () (set v)))))
  (for*/fold ([h h1])
             ([user (in-list '(#t #f))]
              [c+p (links #:with-path? #t #:user? user)]
              #:when (directory-exists? (cdr c+p)))
    (define k (car c+p))
    (define v (path->string (cdr c+p)))
    (hash-update h k (λ (s) (set-add s v)) (λ () (set v)))))

(define (get-completions str [cols (collections)])
  (match (string-split str "/" #:trim? #f)
    [(and frags (list col path path* ...))
     (define (join-str f)
       (string-join (append (drop-right frags 1) (list f)) "/"))
     (let ([dirs (hash-ref cols col (λ () #f))])
       (cond
         [(not dirs) (set)]
         [else
          (for/fold ([s (set)])
                    ([dir (in-set dirs)]
                     #:when (directory-exists? dir))
            (let loop ([path^ (cons path path*)] [dir dir])
              (match path^
                [(list p)
                 (for/fold ([s s])
                           ([f (in-list (directory-list dir))])
                   (cond
                     [(not (string-prefix? (path->string f) p)) s]
                     [(and (file-exists? (build-path dir f))
                           (member (path-get-extension f)
                                   '(#".rkt" #".rhm")))
                      (set-add
                        s
                        (join-str
                          (path->string
                            (path-replace-extension f #""))))]
                     [(and (directory-exists? (build-path dir f))
                           (not (string=? (path->string f) "compiled")))
                      (set-add
                        s
                        (join-str (path->string f)))]
                     [else s]))]
                [(cons "" _) s]
                [(cons p path*)
                 (define more-dir (build-path dir p))
                 (if (directory-exists? more-dir)
                     (loop path* more-dir)
                     s)]
                [else s])))]))]
    [else (set)]))

(provide collections get-completions)

