#lang racket

(module+ test
  (require "../doclib/doc.rkt"
           "../common/path-util.rkt")
  (require profile-flame-graph)

  (define path (normalize-path "../doclib/editor.rkt"))
  (profile
    (let ([d (make-doc (path->uri path) (file->string path))])
      (doc-expand! d))
    #:svg-path "flamegraph.svg"))
