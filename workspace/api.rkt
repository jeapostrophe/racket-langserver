#lang racket/base

;; Public workspace surface. Implementation stays in state.rkt and
;; contribution-store.rkt. Shared binding types are defined in
;; doclib/internal-types.rkt so document analysis does not require this
;; module.

(require "state.rkt"
         "../doclib/internal-types.rkt")

(provide Workspace?
         make-workspace
         workspace-add-folder!
         workspace-remove-folder!
         workspace-contains?
         workspace-set-contribution!
         workspace-remove-path!
         workspace-reference-sources
         workspace-definition-location
         (struct-out Module-Binding)
         (struct-out Doc-Contribution)
         (struct-out Reference-Source))
