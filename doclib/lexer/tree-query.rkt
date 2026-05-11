#lang racket/base

(require "shared.rkt"
         "token-tree.rkt")

;; Structural queries over a Token-Forest. This module depends on the node
;; shapes from token-tree.rkt but does not know how forests were produced.

;; Return a flat list of all nodes in the forest in pre-order (parents
;; before children, depth-first).
(define (token-forest-flattened-nodes forest)
  (define nodes '())

  (define (walk node)
    (set! nodes (cons node nodes))
    (for-each walk (token-node-children node)))

  (for-each walk (Token-Forest-nodes forest))
  (reverse nodes))

;; True when `pos` is inside `node`'s span (inclusive start, exclusive end).
(define (node-contains-pos? node pos)
  (and (<= (token-node-start node) pos)
       (< pos (token-node-end node))))

;; Return the ancestors of the innermost node at `pos`, from node to root.
;; The first element is the node itself, the last is the root. Returns #f
;; if pos is outside all nodes.
(define (token-forest-ancestors-at-pos forest pos)
  (define (search-node node path)
    (define next
      (for/first ([child (in-list (token-node-children node))]
                  #:when (node-contains-pos? child pos))
        child))
    (if next
        (search-node next (cons node path))
        (cons node path)))
  (for/first ([node (in-list (Token-Forest-nodes forest))]
              #:when (node-contains-pos? node pos))
    (search-node node '())))

;; Search the forest for a specific node `target` (compared via eq?) and
;; return the list of nodes from root to `target` inclusive, or #f if not
;; found.
(define (token-forest-node-path forest target)
  (define ancestors (token-forest-ancestors-at-pos forest (token-node-start target)))
  (and ancestors (memq target ancestors)))

;; Find the parent and full path of `target`.
;; Returns two values: (parent path) or (#f path) for a root, or (#f #f)
;; if `target` is not in `forest`.
(define (token-node-parent/path forest target)
  (define path (token-forest-node-path forest target))
  (cond
    [(and path (pair? (cdr path)))
     (values (cadr path) path)]
    [path
     (values #f path)]
    [else
     (values #f #f)]))

;; Find the innermost Token-List that contains `pos`.
;; Example: for the text "(define (f x) (+ x 1))", at a position inside "x"
;; the innermost list is "(f x)".
(define (token-forest-deepest-enclosing-list forest pos)
  (define ancestors (token-forest-ancestors-at-pos forest pos))
  (and ancestors
       (for/first ([node (in-list ancestors)]
                   #:when (Token-List? node))
         node)))

;; Within the deepest list enclosing `pos`, find the first child that is a
;; non-skippable symbol Token-Leaf — i.e., the function/macro/operator
;; name in a form like `(define x 1)`. Returns the LexerTokenSpan or #f.
(define (token-forest-form-head forest pos)
  (define maybe-list-node (token-forest-deepest-enclosing-list forest pos))
  (and maybe-list-node
       ;; pos must be strictly inside the list body.
       ;; Using LexerTokenSpan-end ensures we skip the entire delimiter,
       ;; not just its first character. A position on the delimiter itself
       ;; is a boundary, not inside the body, so it should not match.
       (>= pos (LexerTokenSpan-end (Token-List-open-span maybe-list-node)))
       (for/first ([child (in-list (Token-List-children maybe-list-node))]
                   #:when (and (non-skippable-node? child)
                               (Token-Leaf? child)
                               (token-leaf-type? child 'symbol)))
         (Token-Leaf-span child))))

;; Return a list of (start . end) pairs for every sexp-comment (#;)
;; node in the forest. Walks recursively, so nested sexp-comments
;; under another sexp-comment are not double-counted.
(define (token-forest-sexp-comment-spans forest)
  (define result '())
  (define (walk node)
    (if (sexp-comment-node? node)
        (set! result (cons (token-node-span node) result))
        (for-each walk (token-node-children node))))
  (for-each walk (Token-Forest-nodes forest))
  (reverse result))

(provide token-forest-flattened-nodes
         token-forest-node-path
         token-node-parent/path
         token-forest-ancestors-at-pos
         token-forest-deepest-enclosing-list
         token-forest-form-head
         token-forest-sexp-comment-spans)
