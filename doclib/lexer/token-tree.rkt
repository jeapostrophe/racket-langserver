#lang racket/base

(require "shared.rkt"
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
         parse-token-node
         (struct-out Token-Leaf)
         (struct-out Token-Tree)
         (struct-out Token-Prefix-Tree))
