#lang racket/base
(require json
         racket/contract/base
         racket/match)

;; Apends a message 'new-msg' to the list of pending message 'msgs'.
;; If the new msg is a didChange notification, then a flag will be set
;; on pending didChange msgs that match the new one. This is to prevent
;; redundant syntax checks when the user makes several rapid modifications
;; to the document.
(define (append-message msgs new-msg)
  (define msgs*
    (match new-msg
      ;; If new msg is didChange, then inspect pending msgs for redundant synchecks
      [(hash-table ['method "textDocument/didChange"]
                   ['params (hash-table ['textDocument (hash-table ['uri new-uri])])])
       (define (eq-new-uri? uri)
         (equal? uri new-uri))
       (for/list ([m msgs])
         (match m
           ;; If pending msg matches new msg, then set skip-syncheck flag
           [(hash-table ['method "textDocument/didChange"]
                        ['params (hash-table
                                  ['textDocument (hash-table ['uri (? eq-new-uri?)])])])
            (define params (hash-ref m 'params))
            (hash-set m 'params (hash-set params skip-syncheck #t))]
           ;; Else do not modify
           [_ m]))]
      ;; Else do not inspect pending msgs
      [_ msgs]))
  (append msgs* (list new-msg)))

;; Guaranteed unique key for flagging a syncheck that should be skipped.
;; Injected into the 'params' object so that it can be checked by
;; 'text-document/did-change!' later.
(define skip-syncheck (gensym 'skip-syncheck))

(provide
 skip-syncheck
 (contract-out
  [append-message ((listof jsexpr?) jsexpr? . -> . (listof jsexpr?))]))