#lang racket/base

(require "interface.rkt"
         racket/class
         "../docs-helpers.rkt"
         data/interval-map
         net/url
         drracket/check-syntax)

(provide docs%)

(define docs%
  (class base-service%
    (super-new)
    (define docs (make-interval-map))

    (define/override (get)
      docs)

    (define/override (reset)
      (set! docs (make-interval-map)))

    (define/override (expand start end)
      (interval-map-expand! docs start end))

    (define/override (contract start end)
      (interval-map-contract! docs start end))

    (define/override (syncheck:add-docs-menu _src start end _id _label path def-tag url-tag)
      ;; When start = end, it means the identifier is not found in the source file,
      ;; but exists in expanded syntax. So we shouldn't add an item for it.
      (when (< start end)
        (define path-url (path->url path))
        (define link+tag
          (cond
            [url-tag (struct-copy url path-url [fragment url-tag])]
            [def-tag (struct-copy url path-url [fragment (def-tag->html-anchor-tag def-tag)])]
            [else path-url]))
        (interval-map-set! docs start end (list (url->string link+tag) def-tag))))))

