#lang racket/base

(require "check-syntax.rkt"
         "monitor.rkt"
         "msg-io.rkt"
         "responses.rkt"
         "interfaces.rkt"
         racket/gui
         framework)

;; (struct/c Doc (is-a?/c text%) (is-a?/c build-trace%))
(struct Doc
  (text trace path during-batch-change?) #:transparent #:mutable)

(define (send-diagnostics doc diag-lst)
  (display-message/flush (diagnostics-message (Doc-path doc) diag-lst)))

;; the only place where really run check-syntax
(define (doc-run-check-syntax doc)
  (match-define (list new-trace diags)
    (report-time (check-syntax (Doc-path doc) (Doc-text doc) (Doc-trace doc))))
  (send-diagnostics doc diags)
  (set-Doc-trace! doc new-trace))

(define (lazy-check-syntax doc)
  (when (not (Doc-during-batch-change? doc))
    (doc-run-check-syntax doc)))

(define (new-doc path text)
  (define doc-text (new racket:text%))
  (send doc-text insert text 0)
  (define doc (Doc doc-text #f path #f))
  (lazy-check-syntax doc)
  doc)

(define (doc-reset! doc new-text)
  (define doc-text (Doc-text doc))
  (define doc-trace (Doc-trace doc))

  (send doc-text erase)
  (send doc-trace reset)
  (send doc-text insert new-text 0)
  (lazy-check-syntax doc))

(define (doc-update! doc st-ln st-ch ed-ln ed-ch text)
  (define doc-text (Doc-text doc))
  (define doc-trace (Doc-trace doc))

  (define st-pos (line/char->pos doc-text st-ln st-ch))
  (define end-pos (line/char->pos doc-text ed-ln ed-ch))
  (define old-len (- end-pos st-pos))
  (define new-len (string-length text))
  (cond [(> new-len old-len) (send doc-trace expand end-pos (+ st-pos new-len))]
        [(< new-len old-len) (send doc-trace contract (+ st-pos new-len) end-pos)]
        [else #f])
  (send doc-text insert text st-pos end-pos)
  (lazy-check-syntax doc))

(define-syntax-rule (doc-batch-change doc expr ...)
  (let ()
    (set-Doc-during-batch-change?! doc #t)
    expr ...
    (set-Doc-during-batch-change?! doc #f)
    (lazy-check-syntax doc)))

(provide Doc-text
         Doc-trace
         new-doc
         doc-update!
         doc-reset!
         doc-batch-change)
