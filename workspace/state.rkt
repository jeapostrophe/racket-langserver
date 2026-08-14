#lang racket/base

(require "../common/interfaces.rkt"
         "../common/path-util.rkt"
         "../doclib/internal-types.rkt"
         "contribution-store.rkt"
         racket/contract
         racket/set)

(provide Workspace?
         make-workspace
         workspace-add-folder!
         workspace-remove-folder!
         workspace-contains?
         workspace-set-contribution!
         workspace-remove-path!
         workspace-reference-sources
         workspace-definition-location)

;; Workspace owns folders and immutable accepted contributions.
;; Folder and contribution paths are path? and assumed already simple-form;
;; this layer does not convert them.
;; Keep this lock as a leaf: operations under it must not call document services.
(struct/contract Workspace
  ([lock semaphore?]
   [folders (set/c path? #:kind 'mutable)]
   [contributions Contribution-Store?]))

;; Workspace folder count is normally one and is a small practical constant.
;; A linear scan is simpler and cheaper than maintaining a path index.
(define (folders-contain-path? folders path)
  (for/or ([folder (in-set folders)])
    (directory-contains? folder path)))

(define/contract (make-workspace)
  (-> Workspace?)
  (Workspace (make-semaphore 1)
             (mutable-set)
             (make-contribution-store)))

(define/contract (workspace-add-folder! workspace path)
  (-> Workspace? path? void?)
  (call-with-semaphore
    (Workspace-lock workspace)
    (lambda ()
      (set-add! (Workspace-folders workspace) path))))

(define (purge-uncovered-contributions! workspace)
  (define folders (Workspace-folders workspace))
  (define contributions (Workspace-contributions workspace))
  (for ([path (in-list (contribution-store-source-paths contributions))]
        #:unless (folders-contain-path? folders path))
    (contribution-store-remove-source! contributions path)))

(define/contract (workspace-remove-folder! workspace path)
  (-> Workspace? path? void?)
  (call-with-semaphore
    (Workspace-lock workspace)
    (lambda ()
      (set-remove! (Workspace-folders workspace) path)
      (purge-uncovered-contributions! workspace))))

(define/contract (workspace-contains? workspace path)
  (-> Workspace? path? boolean?)
  (call-with-semaphore
    (Workspace-lock workspace)
    (lambda ()
      (folders-contain-path? (Workspace-folders workspace) path))))

(define/contract (workspace-set-contribution! workspace contribution)
  (-> Workspace? Doc-Contribution? void?)
  (define path (Doc-Contribution-path contribution))
  (call-with-semaphore
    (Workspace-lock workspace)
    (lambda ()
      (when (folders-contain-path? (Workspace-folders workspace) path)
        (contribution-store-add! (Workspace-contributions workspace) contribution)))))

(define/contract (workspace-remove-path! workspace path)
  (-> Workspace? path? void?)
  (call-with-semaphore
    (Workspace-lock workspace)
    (lambda ()
      (contribution-store-remove-source! (Workspace-contributions workspace) path))))

(define/contract (workspace-reference-sources workspace module-binding)
  (-> Workspace? Module-Binding? (listof Reference-Source?))
  (call-with-semaphore
    (Workspace-lock workspace)
    (lambda ()
      (contribution-store-reference-sources (Workspace-contributions workspace) module-binding))))

(define/contract (workspace-definition-location workspace module-binding)
  (-> Workspace? Module-Binding? (or/c Location? #f))
  (call-with-semaphore
    (Workspace-lock workspace)
    (lambda ()
      (contribution-store-definition-location
        (Workspace-contributions workspace)
        module-binding))))
