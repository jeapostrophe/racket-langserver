#lang racket/base

;; Effects of accepted analysis: document projections, workspace contributions,
;; and diagnostics. The controller chooses eligibility and destination. Buffer
;; operations below require its SafeDoc write lock; they never acquire it again.
(require racket/contract
         "../../common/json-util.rkt"
         "../../common/source.rkt"
         "../../doclib/doc.rkt"
         "../../workspace/state.rkt"
         "../analysis.rkt"
         "../safedoc.rkt")

(provide Analysis-Publisher?
         make-analysis-publisher
         set-analysis-notifier!
         publish-document-analysis!
         publish-document-resyntax!
         withdraw-document-analysis!)

(struct/contract Analysis-Publisher
  ([workspace Workspace?]
   [(notify #:mutable) (-> string? any/c any/c)]))

(define (make-analysis-publisher workspace)
  (Analysis-Publisher workspace (lambda (_method _params) (void))))

(define (set-analysis-notifier! publisher notify)
  (set-Analysis-Publisher-notify! publisher notify))

(define (publish-diagnostics! publisher uri diagnostics)
  ((Analysis-Publisher-notify publisher) "textDocument/publishDiagnostics"
                                         (hasheq 'uri uri 'diagnostics diagnostics)))

;; A restored product has no fresh completion. Source errors clear exact
;; availability while preserving the old trace for approximate local queries.
(define (publish-document-analysis! publisher sd snapshot product [completion #f])
  (define doc (SafeDoc-doc sd))
  (cond
    [(Analysis-Product-succeeded? product)
     (define contribution (Analysis-Product-contribution product))
     (doc-update-trace! doc (Analysis-Product-trace product) contribution (Doc-version doc))
     (set-SafeDoc-available! sd (Available-Analysis snapshot product completion))
     (when contribution
       (workspace-set-contribution! (Analysis-Publisher-workspace publisher) contribution))]
    [else (set-SafeDoc-available! sd #f)])
  (publish-diagnostics! publisher (Doc-uri doc) (Analysis-Product-diagnostics product)))

;; Completion identity distinguishes same-revision retries. Old Resyntax
;; findings cannot apply to a restored product or a later accepted attempt.
(define (publish-document-resyntax! publisher sd parent-completion results)
  (define available (SafeDoc-available sd))
  (when (and available (eq? parent-completion (Available-Analysis-completion-evt available)))
    (define doc (SafeDoc-doc sd))
    (doc-update-resyntax-result! doc results)
    (publish-diagnostics! publisher (Doc-uri doc) (->jsexpr (doc-diagnostics doc)))))

;; Keep the opened URI spelling; a path cannot reconstruct it.
(define (withdraw-document-analysis! publisher source uri)
  (when (source-file-path source)
    (workspace-remove-path! (Analysis-Publisher-workspace publisher) (source-file-path source)))
  (publish-diagnostics! publisher uri '()))
