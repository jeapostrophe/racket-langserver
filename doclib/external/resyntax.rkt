#lang racket/base

(require racket/runtime-path)

(provide run-resyntax
         resyntax-available?
         resyntax-collection-present?)

(define-runtime-module-path-index engine-path "resyntax-engine.rkt")

(define resolved-engine #f)

;; Lazily resolves resyntax-engine.rkt exports on first use so importing this
;; facade does not load heavy macro and refactoring libraries.
(define (load-engine!)
  (or resolved-engine
      (let ()
        (define avail? (dynamic-require engine-path 'resyntax-available?))
        (define run (dynamic-require engine-path 'run-resyntax))
        (define engine (cons avail? run))
        (set! resolved-engine engine)
        engine)))

(define (resyntax-available?)
  ((car (load-engine!))))

(define (run-resyntax text uri)
  ((cdr (load-engine!)) text uri))

;; Presence is only a hint; the engine checks its imports in the worker.
(define (resyntax-collection-present?)
  (and (collection-file-path "main.rkt" "resyntax" #:fail (lambda (_) #f)) #t))
