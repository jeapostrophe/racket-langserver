#lang racket/base

(provide rope-from-string
         rope-replace
         rope-get-text
         rope->string
         rope-chars
         rope-ref
         rope-line-start-offset
         rope-line-end-offset
         rope-offset->line)

(require racket/match
         (for-syntax racket/base))

;; Immutable rope with string leaves.
;; For CRLF, Only `#\newline `contributes to line structure,
;; `#\return `stays in the leaf text as an ordinary character.

;; Rope is one of:
;; - RopeLeaf
;; - RopeNode

;; RopeLeaf is a leaf node containing a string segment. It stores the offsets of
;; `#\newline` characters for efficient line-based operations.
;; `text` : String
;; `newline-offsets` : (Vectorof Exact-Nonnegative-Integer), Offsets of each `#\newline` in `text`.
;; `chars` : Exact-Nonnegative-Integer, Equal to `(string-length text)`.
;; `newlines` : Exact-Nonnegative-Integer, Equal to `(vector-length newline-offsets)`.
(struct RopeLeaf
  (text
    newline-offsets
    chars
    newlines)
  #:transparent)

;; RopeNode is an internal node that combines two child ropes. It caches the total
;; character count, newline count, and subtree height for efficient operations and balancing.
;; The balancing strategy is similar to AVL trees, where the height difference between left
;; and right subtrees is at most 1.
;; `left` : Rope
;; `right` : Rope
;; `chars` : Exact-Nonnegative-Integer, Total character count in this subtree.
;; `newlines` : Exact-Nonnegative-Integer, Total number of `#\newline` characters in this subtree.
;; `height` : Exact-Nonnegative-Integer, Cached subtree height used for balancing.
(struct RopeNode
  (left
    right
    chars
    newlines
    height)
  #:transparent)

(define-match-expander RopeNode*
  (lambda (stx)
    (syntax-case stx ()
      [(_ l r)
       #'(struct* RopeNode ([left l] [right r]))])))

(define (make-RopeLeaf #:text text
                       #:newline-offsets newline-offsets
                       #:chars chars
                       #:newlines newlines)
  (RopeLeaf text newline-offsets chars newlines))

(define (make-RopeNode #:left left
                       #:right right
                       #:chars chars
                       #:newlines newlines
                       #:height height)
  (RopeNode left right chars newlines height))

;; Preferred number of characters in a leaf.
(define good-leaf-size 512)
;; Hard upper bound on a leaf size before it should be considered too large.
(define limit-leaf-size 1024)
;; A leaf at or below this size is considered small enough that merging it into
;; a neighboring leaf is usually worthwhile, as long as the result stays within
;; `limit-leaf-size`.
(define small-leaf-size 256)

;; The empty rope is represented as a `RopeLeaf` with no text. It plays the role
;; of the empty tree / null child, so its height is 0 instead of 1.
(define empty-rope
  (make-RopeLeaf #:text ""
                 #:newline-offsets #()
                 #:chars 0
                 #:newlines 0))

(define (rope-chars rope)
  (if (RopeLeaf? rope)
      (RopeLeaf-chars rope)
      (RopeNode-chars rope)))

(define (rope-newlines rope)
  (if (RopeLeaf? rope)
      (RopeLeaf-newlines rope)
      (RopeNode-newlines rope)))

(define (rope-height rope)
  (if (RopeLeaf? rope)
      (if (zero? (RopeLeaf-chars rope)) 0 1)
      (RopeNode-height rope)))

;; For a balanced internal node, the balance factor is one of -1, 0, or 1.
;; During a local AVL update, it can temporarily become -2 or 2 before
;; `rebalance` restores the invariant.
;; For arbitrary pairs of ropes ,the difference can be larger;
;; `rope-concat` handles those cases recursively.
(define (rope-balance-factor left right)
  (- (rope-height right) (rope-height left)))

(define (rope-node-balance-factor node)
  (match-define (RopeNode* left right) node)
  (rope-balance-factor left right))

(define (rope-empty? rope)
  (zero? (rope-chars rope)))

(define (count-chunks total-size chunk-size)
  (quotient (+ total-size (sub1 chunk-size))
            chunk-size))

(define (string->local-newline-offsets str)
  (for/vector ([character (in-string str)]
               [offset (in-naturals)]
               #:when (char=? character #\newline))
    offset))

(define (make-leaf text)
  (define chars (string-length text))
  (if (zero? chars)
      empty-rope
      (let ([newline-offsets (string->local-newline-offsets text)])
        (make-RopeLeaf #:text text
                       #:newline-offsets newline-offsets
                       #:chars chars
                       #:newlines (vector-length newline-offsets)))))

(define (make-node left right)
  (make-RopeNode #:left left
                 #:right right
                 #:chars (+ (rope-chars left) (rope-chars right))
                 #:newlines (+ (rope-newlines left) (rope-newlines right))
                 #:height (add1 (max (rope-height left) (rope-height right)))))

(define (mergeable-leaves? left right)
  (define combined-chars (+ (RopeLeaf-chars left) (RopeLeaf-chars right)))
  (and (<= combined-chars limit-leaf-size)
       (or (<= (RopeLeaf-chars left) small-leaf-size)
           (<= (RopeLeaf-chars right) small-leaf-size)
           (<= combined-chars good-leaf-size))))

;; Combine two ropes without recursive rebalancing.
;; Empty ropes are ignored, and adjacent mergeable leaves are coalesced into a
;; single leaf to avoid unnecessary internal nodes.
(define (join-ropes left right)
  (cond
    [(rope-empty? left) right]
    [(rope-empty? right) left]
    [(and (RopeLeaf? left)
          (RopeLeaf? right)
          (mergeable-leaves? left right))
     (make-leaf (string-append (RopeLeaf-text left) (RopeLeaf-text right)))]
    [else (make-node left right)]))

(define (rotate-left l r)
  (match-define (RopeNode* rl rr) r)
  (join-ropes (join-ropes l rl) rr))

(define (rotate-right l r)
  (match-define (RopeNode* ll lr) l)
  (join-ropes ll (join-ropes lr r)))

(define (rotate-left-right l r)
  (match-define (RopeNode* ll (RopeNode* lrl lrr)) l)
  (join-ropes (join-ropes ll lrl) (join-ropes lrr r)))

(define (rotate-right-left l r)
  (match-define (RopeNode* (RopeNode* rll rlr) rr) r)
  (join-ropes (join-ropes l rll) (join-ropes rlr rr)))

;; Rebuild a rope from `left` and `right`, applying the needed AVL rotation if
;; one side is taller by more than 1. In the imbalanced cases, the heavier side
;; must already be a `RopeNode`, so the rotation helpers can destructure it
;; without an extra type check.
(define (rebalance left right)
  (define balance-factor (rope-balance-factor left right))
  (cond
    [(< balance-factor -1)
     (if (<= (rope-node-balance-factor left) 0)
         (rotate-right left right)
         (rotate-left-right left right))]
    [(> balance-factor 1)
     (if (>= (rope-node-balance-factor right) 0)
         (rotate-left left right)
         (rotate-right-left left right))]
    [else (join-ropes left right)]))

;; Concatenate two balanced ropes while preserving the balance invariant.
;; The two input ropes may differ in height by more than 2. In that case, this
;; descends into the heavier side until the join becomes local, then rebuilds
;; and rebalances on the way back up.
;; In the recursive case, the returned
;; subtree can grow by at most 1 in height relative to the boundary subtree it
;; replaces, so one local `rebalance` at the parent is sufficient.
(define (rope-concat left right)
  (define balance-factor (rope-balance-factor left right))
  (cond
    [(rope-empty? left) right]
    [(rope-empty? right) left]
    [(< balance-factor -1)
     (match-define (RopeNode* left-left left-right) left)
     (rebalance left-left (rope-concat left-right right))]
    [(> balance-factor 1)
     (match-define (RopeNode* right-left right-right) right)
     (rebalance (rope-concat left right-left) right-right)]
    [else (join-ropes left right)]))

;; Build a balanced rope from the half-open leaf slice [start, end).
;; The input leaves are assumed to already be valid rope leaves.
(define (build-balanced-rope leaves start end)
  (define leaf-count (- end start))
  (cond
    [(zero? leaf-count) empty-rope]
    [(= leaf-count 1) (vector-ref leaves start)]
    [else
     (define mid (+ start (quotient leaf-count 2)))
     (join-ropes (build-balanced-rope leaves start mid)
                 (build-balanced-rope leaves mid end))]))

(define (rope-from-string str)
  (define chars (string-length str))
  (cond
    [(zero? chars) empty-rope]
    [(<= chars good-leaf-size) (make-leaf str)]
    [else
     (define leaf-count (count-chunks chars good-leaf-size))
     (define leaves
       (for/vector #:length leaf-count ([start (in-range 0 chars good-leaf-size)])
         (define end (min chars (+ start good-leaf-size)))
         (make-leaf (substring str start end))))
     (build-balanced-rope leaves 0 leaf-count)]))

;; Split `rope` at character offset `offset`.
;; Returns two ropes whose concatenation is the original rope, with the split
;; point between them.
(define (rope-split-at-offset rope offset)
  (define chars (rope-chars rope))
  (cond
    [(rope-empty? rope) (values empty-rope empty-rope)]
    [(<= offset 0) (values empty-rope rope)]
    [(>= offset chars) (values rope empty-rope)]
    [(RopeLeaf? rope)
     (define text (RopeLeaf-text rope))
     (values (make-leaf (substring text 0 offset))
             (make-leaf (substring text offset chars)))]
    [else
     (match-define (RopeNode* left right) rope)
     (define left-chars (rope-chars left))
     (cond
       [(= offset left-chars) (values left right)]
       [(< offset left-chars)
        (define-values (split-left split-right)
          (rope-split-at-offset left offset))
        (values split-left (rope-concat split-right right))]
       [else
        (define-values (split-left split-right)
          (rope-split-at-offset right (- offset left-chars)))
        (values (rope-concat left split-left) split-right)])]))

;; Return the absolute character offset of the `newline-index`th newline.
;; `newline-index` is zero-based over the newline characters in the rope.
(define (rope-nth-newline-offset rope newline-index)
  (match rope
    [(struct* RopeLeaf ([newline-offsets newline-offsets]))
     (vector-ref newline-offsets newline-index)]
    [(RopeNode* left right)
     (define left-newlines (rope-newlines left))
     (if (< newline-index left-newlines)
         (rope-nth-newline-offset left newline-index)
         (+ (rope-chars left)
            (rope-nth-newline-offset right (- newline-index left-newlines))))]))

;; Return the first index whose value is greater than or equal to `target`.
(define (vector-lower-bound sorted-vector target)
  (let loop ([low 0]
             [high (vector-length sorted-vector)])
    (if (= low high)
        low
        (let ([mid-index (quotient (+ low high) 2)])
          (if (< (vector-ref sorted-vector mid-index) target)
              (loop (add1 mid-index) high)
              (loop low mid-index))))))

;; Count how many newline characters occur before `offset`.
;; The count is over the half-open prefix [0, offset).
;; This identifies the zero-based line that contains `offset`.
(define (count-newlines-before-offset rope offset)
  (match rope
    [(struct* RopeLeaf ([newline-offsets newline-offsets]))
     (vector-lower-bound newline-offsets offset)]
    [(RopeNode* left right)
     (define left-chars (rope-chars left))
     (if (< offset left-chars)
         (count-newlines-before-offset left offset)
         (+ (rope-newlines left)
            (count-newlines-before-offset right (- offset left-chars))))]))

(define (rope-ref rope offset)
  (match rope
    [(struct* RopeLeaf ([text text]))
     (string-ref text offset)]
    [(RopeNode* left right)
     (define left-chars (rope-chars left))
     (if (< offset left-chars)
         (rope-ref left offset)
         (rope-ref right (- offset left-chars)))]))

(define (write-rope-range rope start end out)
  (when (< start end)
    (match rope
      [(struct* RopeLeaf ([text text]))
       (write-string text out start end)]
      [(RopeNode* left right)
       (define left-chars (rope-chars left))
       (when (< start left-chars)
         (write-rope-range left start (min end left-chars) out))
       (when (> end left-chars)
         (write-rope-range right (max 0 (- start left-chars)) (- end left-chars) out))])))

(define (rope->string rope)
  (define out (open-output-string))
  (write-rope-range rope 0 (rope-chars rope) out)
  (get-output-string out))

(define (line-start-offset rope line)
  (if (zero? line)
      0
      (add1 (rope-nth-newline-offset rope (sub1 line)))))

(define (rope-line-count rope)
  (add1 (rope-newlines rope)))

(define (rope-replace rope start-offset end-offset str)
  (define-values (left rest)
    (rope-split-at-offset rope start-offset))
  (define-values (_discard right)
    (rope-split-at-offset rest (- end-offset start-offset)))
  (rope-concat (rope-concat left (rope-from-string str))
               right))

(define (rope-get-text rope [start 0] [end (rope-chars rope)])
  (define out (open-output-string))
  (write-rope-range rope start end out)
  (get-output-string out))

(define (rope-line-end-offset rope line)
  (if (< line (rope-newlines rope))
      (rope-nth-newline-offset rope line)
      (rope-chars rope)))

(define (rope-line-start-offset rope line)
  (if (>= line (rope-line-count rope))
      (rope-chars rope)
      (line-start-offset rope line)))

(define (rope-offset->line rope offset)
  (count-newlines-before-offset rope offset))

