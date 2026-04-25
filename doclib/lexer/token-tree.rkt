#lang racket/base

(require "shared.rkt"
         racket/list
         racket/match)

;; Token-Leaf: a single token span, representing a leaf in the token tree.
(struct Token-Leaf (span) #:transparent)
;; Token-Tree: a delimited form, with an open parenthesis span,
;;             a list of child nodes ,and an optional close parenthesis span.
(struct Token-Tree (open-span children close-span) #:transparent)
;; Token-Prefix-Tree: a prefix token (quote-family, syntax quote-family,
;;                    or `#;`) plus the skippable trivia after it and an
;;                    optional operand node.
(struct Token-Prefix-Tree (prefix-span skippable-nodes child) #:transparent)

(define (token-node? value)
  (or (Token-Leaf? value)
      (Token-Tree? value)
      (Token-Prefix-Tree? value)))

(define (sexp-comment-node? value)
  (and (Token-Prefix-Tree? value)
       (eq? 'sexp-comment
            (LexerTokenSpan-type (Token-Prefix-Tree-prefix-span value)))))

(define (skippable-span? span)
  (memq (LexerTokenSpan-type span) '(white-space comment)))

(define (non-skippable-node? node)
  (not (or (and (Token-Leaf? node)
                (skippable-span? (Token-Leaf-span node)))
           (sexp-comment-node? node))))

;; Return up to `n` meaningful nodes from an already-built token tree, ignoring
;; whitespace, ordinary comments, and complete `#;` sexp-comment nodes.
(define (read-next-non-skippable-nodes nodes n)
  (let loop ([nodes nodes]
             [read-nodes '()])
    (cond
      [(= (length read-nodes) n) (reverse read-nodes)]
      [(null? nodes) (reverse read-nodes)]
      [else
       (define node (car nodes))
       (if (non-skippable-node? node)
           (loop (cdr nodes) (cons node read-nodes))
           (loop (cdr nodes) read-nodes))])))

;; Parse up to `n` meaningful nodes directly from the token span vector, returning
;; the parsed nodes and the index where parsing stopped. Skippable spans before
;; each node are consumed but not included in the returned node list.
(define (read-next-non-skippable-nodes/spans spans idx n)
  (let loop ([idx idx]
             [read-nodes '()])
    (cond
      [(= (length read-nodes) n)
       (values (reverse read-nodes) idx)]
      [else
       (define-values (_skippable-nodes next-idx)
         (parse-skippable-node spans idx))
       (cond
         [(not (span-at spans next-idx))
          (values (reverse read-nodes) next-idx)]
         [else
          (define-values (node node-idx)
            (parse-token-node spans next-idx))
          (loop node-idx (cons node read-nodes))])])))

(define (token-node-start node)
  (match node
    [(Token-Leaf span) (LexerTokenSpan-start span)]
    [(Token-Tree open-span _children _close-span)
     (LexerTokenSpan-start open-span)]
    [(Token-Prefix-Tree prefix-span _skippable-nodes _child)
     (LexerTokenSpan-start prefix-span)]))

(define (token-node-end node)
  (match node
    [(Token-Leaf span) (LexerTokenSpan-end span)]
    [(Token-Tree open-span children close-span)
     (cond
       [close-span (LexerTokenSpan-end close-span)]
       [(null? children) (LexerTokenSpan-end open-span)]
       [else (token-node-end (last children))])]
    [(Token-Prefix-Tree prefix-span _skippable-nodes child)
     (if child
         (token-node-end child)
         (LexerTokenSpan-end prefix-span))]))

;; Skip over whitespaces and comments, accumulating them into `nodes` so they can be
;; preserved in the tree if needed.
(define (parse-skippable-node spans idx [nodes '()])
  (define maybe-span (span-at spans idx))
  (match* (maybe-span (and maybe-span (LexerTokenSpan-type maybe-span)))
    [(#f #f)
     (values (reverse nodes) idx)]
    [(span (or 'white-space 'comment))
     (parse-skippable-node spans (add1 idx) (cons (Token-Leaf span) nodes))]
    [(_ 'sexp-comment)
     (define-values (comment-node comment-idx)
       (parse-token-node spans idx))
     (parse-skippable-node spans comment-idx (cons comment-node nodes))]
    [(_ _)
     (values (reverse nodes) idx)]))

;; Parse a prefix token and its operand, skipping over any whitespace/comments
;; in between.
(define (parse-prefix-node spans idx prefix-span)
  (define-values (skippable-nodes next-idx)
    (parse-skippable-node spans (add1 idx)))
  (match (span-at spans next-idx)
    [#f
     (values (Token-Prefix-Tree prefix-span skippable-nodes #f) next-idx)]
    [_
     (define-values (child child-idx)
       (parse-token-node spans next-idx))
     (values (Token-Prefix-Tree prefix-span skippable-nodes child) child-idx)]))

;; Parse a list form, recursively parsing its children until the closing parenthesis is found.
(define (parse-list-node spans idx open-span children)
  (define maybe-span (span-at spans idx))
  (match* (maybe-span (and maybe-span (LexerTokenSpan-type maybe-span)))
    [(#f #f)
     (values (Token-Tree open-span (reverse children) #f) idx)]
    [(close-span 'close-paren)
     (values (Token-Tree open-span (reverse children) close-span) (add1 idx))]
    [(_ _)
     (define-values (child child-idx)
       (parse-token-node spans idx))
     (parse-list-node spans child-idx open-span (cons child children))]))

(define (parse-token-node spans idx)
  (define span (vector-ref spans idx))
  (match (LexerTokenSpan-type span)
    [(or 'quote
         'quasiquote
         'unquote
         'unquote-splicing
         'syntax-quote
         'syntax-quasiquote
         'syntax-unquote
         'syntax-unquote-splicing
         'sexp-comment)
     (parse-prefix-node spans idx span)]
    ['open-paren
     (parse-list-node spans (add1 idx) span '())]
    [_
     (values (Token-Leaf span) (add1 idx))]))

(provide token-node?
         sexp-comment-node?
         read-next-non-skippable-nodes
         read-next-non-skippable-nodes/spans
         token-node-start
         token-node-end
         parse-skippable-node
         parse-token-node
         (struct-out Token-Leaf)
         (struct-out Token-Tree)
         (struct-out Token-Prefix-Tree))
