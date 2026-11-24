#lang racket/base

(require racket/match)

(provide define-avl-operations)

;; =============================================================================
;; Reusable Structural AVL Engine
;; =============================================================================
;;
;; define-avl-operations generates purely structural AVL rotations, rebalancing,
;; and Adams/Tarjan height-descending concatenation.
;;
;; Required Tree Representation:
;; - Nonempty leaves have height 1.
;; - Empty trees have height 0 and satisfy (empty-tree? t).
;; - Internal nodes have two nonempty children; `Node*` exposes (Node* left right)
;;   in O(1) time.
;; - Cached heights must strictly agree with the physical tree structure:
;;     height(leaf) = 1
;;     height(node) = 1 + max(height(left), height(right))
;;
;; Required Constructor Contract:
;; - (make-node l r) must be strictly structural in O(1) time:
;;     tree-height(make-node(l, r)) = 1 + max(tree-height(l), tree-height(r))
;; - It MUST NOT coalesce leaves, drop empty trees, or mutate children.
;;
;; Operation Preconditions:
;; - (tree-height t): O(1), returns 0 for empty trees.
;; - (empty-tree? t): O(1) predicate.
;; - rebalance: Assumes L and R are NONEMPTY balanced AVL trees with |h(R) - h(L)| <= 2.
;; - tree-concat-raw: Assumes L and R are balanced AVL trees of arbitrary heights.
;;
;; Scope of Guarantees:
;; - The height bound height(result) in { max(hL, hR), max(hL, hR) + 1 } and the
;;   O(1 + |h(R) - h(L)|) step bound apply strictly to `tree-concat-raw`.
;; - Any outer operation that coalesces boundary leaves or sentinels (such as
;;   gap coalescing in interval ropes or chunk merging in text ropes) can reduce
;;   height below max(hL, hR) and incur additional boundary traversal costs.
;; =============================================================================

(define-syntax-rule (define-avl-operations
                      #:node-match Node*
                      #:make-node make-node
                      #:height tree-height
                      #:empty? empty-tree?
                      #:tree-concat tree-concat-raw)
  (begin
    (define (rotate-left l r)
      (match-define (Node* rl rr) r)
      (make-node (make-node l rl) rr))

    (define (rotate-right l r)
      (match-define (Node* ll lr) l)
      (make-node ll (make-node lr r)))

    (define (rotate-left-right l r)
      (match-define (Node* ll (Node* lrl lrr)) l)
      (make-node (make-node ll lrl) (make-node lrr r)))

    (define (rotate-right-left l r)
      (match-define (Node* (Node* rll rlr) rr) r)
      (make-node (make-node l rll) (make-node rlr rr)))

    (define (rebalance left right)
      (define factor (- (tree-height right) (tree-height left)))
      (cond
        [(< factor -1)
         (match-define (Node* ll lr) left)
         (if (<= (- (tree-height lr) (tree-height ll)) 0)
             (rotate-right left right)
             (rotate-left-right left right))]
        [(> factor 1)
         (match-define (Node* rl rr) right)
         (if (>= (- (tree-height rr) (tree-height rl)) 0)
             (rotate-left left right)
             (rotate-right-left left right))]
        [else (make-node left right)]))

    ;; Adams/Tarjan height-descending concatenation of two balanced AVL trees.
    ;; When heights differ by more than 1, descends along the spine of the taller
    ;; tree until heights are compatible (|factor| <= 1), then rebalances on the
    ;; way back up in O(1 + |h_R - h_L|) time.
    (define (tree-concat-raw left right)
      (cond
        [(empty-tree? left) right]
        [(empty-tree? right) left]
        [else
         (define factor (- (tree-height right) (tree-height left)))
         (cond
           [(< factor -1)
            (match-define (Node* ll lr) left)
            (rebalance ll (tree-concat-raw lr right))]
           [(> factor 1)
            (match-define (Node* rl rr) right)
            (rebalance (tree-concat-raw left rl) rr)]
           [else (make-node left right)])]))))
