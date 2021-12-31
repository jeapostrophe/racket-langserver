#lang racket/base

(require scribble/blueboxes
         setup/xref
         racket/class
         racket/list
         racket/dict
         setup/collects
         racket/string
         scribble/xref
         net/url-string
         racket/format)

(define the-bluebox-cache (make-blueboxes-cache #t))
(define pkg-cache (make-hash))

(define (find-containing-paren pos text)
  (define l (string-length text))
  (cond
    [(>= pos l) #f]
    [else
     (let loop ([i pos] [p 0])
       (cond
         [(< i 0) #f]
         [(or (char=? (string-ref text i) #\() (char=? (string-ref text i) #\[))
          (if (> p 0) (loop (- i 1) (- p 1)) i)]
         [(or (char=? (string-ref text i) #\)) (char=? (string-ref text i) #\]))
          (loop (- i 1) (+ p 1))]
         [else (loop (- i 1) p)]))]))

(define (id-to-tag id trace)
  ;; partial reimplementation of private method compute-tag+rng
  ;; in drracket/private/syncheck/blueboxes-gui.rkt
  (define xref (load-collections-xref))
  (define mps
    (for/list ([(k require-candidate) (in-dict (send trace get-requires))])
      (path->module-path require-candidate #:cache pkg-cache)))
  (for/or ([mp (in-list mps)])
    (define definition-tag (xref-binding->definition-tag xref (list mp (string->symbol id)) #f))
    (cond
      [definition-tag
        (define-values (path url-tag) (xref-tag->path+anchor xref definition-tag))
        (if path definition-tag #f)]
      [else #f])))

(define (get-docs-for-tag tag)
  (define bb-strs (fetch-blueboxes-strs tag #:blueboxes-cache the-bluebox-cache))
  (cond [bb-strs
         (define strs (drop bb-strs 1))
         (define index (let loop ((strs strs) (i 0))
                         (cond
                           [(>= i (length strs)) #f]
                           [(string-prefix? (list-ref strs i) "(") (loop strs (+ i 1))]
                           [else i])))
         (cond [index (list (take strs index) (string-join (if index (drop strs index) strs) "\n"))]
               [else (list strs #f)])]
        [else (list #f #f)]))

;; Examples:
;; Input: "file:///C:/Program Files/Racket/doc/reference/module.html#(form._((quote._~23~25kernel)._module))" #f
;; Output: https://docs.racket-lang.org/reference/module.html#%28form._%28%28quote._%7E23%7E25kernel%29._module%29%29
;;  (i.e. https://docs.racket-lang.org/ + left trimmed `url`)
;; Input: "pairs.html#(def._((lib._racket/list..rkt)._add-between))" "C:/Program Files/Racket/doc/reference/strings.html"
;; Output: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._add-between%29%29
;;  (i.e. https://docs.racket-lang.org/ + /reference/ from `docs-path` + `url`)
(define (make-proper-url-for-online-documentation url [docs-path #f])
  (define online-docs-url "https://docs.racket-lang.org/")
  (define (absolute-web-url? url) (and (string-contains? url "://") (not (string-prefix? url "file"))))
  (define (get-relative-docs-url url) ;; e.g. "reference/module.html#(form._((quote._~23~25kernel)._module))"
    (last (string-split url #rx"/doc/(racket/)?"))) ; "(racket/)?" in case docs are installed in 'usr/share/doc/racket' on linux
  (define (strip-off-last-path-segment url) (string-join (drop-right (string-split url "/") 1) "/" #:after-last "/"))
  (define (encode-url url-string)
    (define url-struct (string->url url-string))
    ;; particularly encode chars '(', ')' and '~' from Markdown. Both VSCode's and Atom's Md parsers don't like them in links.
    (current-url-encode-mode 'unreserved)
    (define encoded-url (string-replace (url->string url-struct) "~" "%7E"))
    ;; Rarely, there are `redirecting` links that require putting `&` back in query to work properly
    (string-replace encoded-url "&amp;" "&"))

  (define encoded-url (encode-url url))
  (cond
    [(absolute-web-url? encoded-url) encoded-url]
    [docs-path
     (define ending (get-relative-docs-url docs-path))
     (~a online-docs-url
         (if (or (string-prefix? encoded-url "#") (zero? (string-length encoded-url)))
             ending
             (strip-off-last-path-segment ending))
         encoded-url)]
    [else (~a online-docs-url (get-relative-docs-url encoded-url))]))

;; Example: '(def ((quote #%kernel) hasheq)) => "(def._((quote._~23~25kernel)._hasheq))"
;; mostly a copy of a closed function `anchor-name` in `scribble-lib/scribble/html-render.rkt`
(define (def-tag->html-anchor-tag v)
  (define (encode-byte b) (string-append (if (< b 16) "~0" "~") (number->string b 16)))
  (define (encode-bytes str) (string->bytes/utf-8 (encode-byte (bytes-ref str 0))))
  (let* ([v (string->bytes/utf-8 (format "~a" v))]
         [v (regexp-replace* #rx#"[A-Z.]" v #".&")]
         [v (regexp-replace* #rx#" " v #"._")]
         [v (regexp-replace* #rx#"\"" v #".'")]
         [v (regexp-replace* #rx#"[^-a-zA-Z0-9_!+*'()/.,]" v encode-bytes)])
    (bytes->string/utf-8 v)))


(provide find-containing-paren
         get-docs-for-tag
         id-to-tag
         make-proper-url-for-online-documentation
         def-tag->html-anchor-tag)
