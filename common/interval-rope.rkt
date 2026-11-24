#lang racket/base

;; Immutable AVL interval rope with relative spans and unmapped gap sentinels.
;; Aligned in structure, rotation helpers, and immutability with
;; doclib/textbuffer/textbuffer-rope.rkt.
;;
;; Design:
;; 1. Immutability:
;;    IntervalRope is a pure value. All transformation functions (set, remove,
;;    expand, contract, update, cons) return a new IntervalRope without mutating
;;    existing nodes or wrappers. Snapshots and concurrent place reads require
;;    zero copying or synchronization.
;;
;; 2. Relative Spans:
;;    Internal nodes and leaves store strictly relative widths (spans) rather
;;    than absolute positions. Absolute coordinates exist only via IntervalRope-origin
;;    and are accumulated dynamically during tree traversal. This allows index
;;    shifts (such as expand and contract) to execute in O(1) time when shifting
;;    at or before the origin, and O(log N) time when shifting inside the tree.
;;
;; 3. Unmapped Gap Representation:
;;    Unmapped coordinates are represented by an unexported `*gap*` sentinel.
;;    Contiguous gaps are always coalesced at join boundaries, so storage
;;    remains strictly proportional to the number of live mapped intervals.
;;
;; 4. Direct Queries & Transformations:
;;    Instead of exposing dictionary or iterator complexity, the rope provides
;;    direct point lookup (interval-rope-ref, interval-rope-ref/bounds), range
;;    overlap queries (interval-rope-overlap-values), and sequence export
;;    (interval-rope->list).

(require "avl-operations.rkt"
         (only-in racket/promise
                  delay
                  force)
         racket/match
         (for-syntax racket/base))

(provide empty-interval-rope
         interval-rope
         interval-rope?
         interval-rope-empty?
         interval-rope-count
         interval-rope-span
         interval-rope-origin
         ;; Point and range queries
         interval-rope-ref
         interval-rope-ref/bounds
         interval-rope-overlap-values
         ;; Pure transformations
         interval-rope-set
         interval-rope-remove
         interval-rope-expand
         interval-rope-expand/extend
         interval-rope-contract
         interval-rope-update
         interval-rope-cons
         ;; Bulk import and export
         adopt-interval-snapshot
         interval-rope-for-each
         interval-rope->list
         in-interval-rope)

;; Unexported sentinel values for unmapped gaps and absent default values.
(define *gap* (gensym '*gap*))
(define *absent* (gensym '*absent*))

;; -----------------------------------------------------------------------------
;; Tree Records & Match Expanders
;; -----------------------------------------------------------------------------

;; A Leaf represents a contiguous region of relative width `span`.
;; `value` is either user data or the unexported `*gap*` sentinel.
;; Leaves store no absolute coordinates.
(struct IntervalLeaf (span value) #:transparent)

;; A Node balances two non-empty subtrees.
;; - `span`: relative width of the subtree (+ (tree-span left) (tree-span right))
;; - `height`: 1 + (max (tree-height left) (tree-height right))
;; - `count`: number of mapped (non-*gap*) leaves in this subtree
;; Nodes store no absolute coordinates.
(struct IntervalNode (left right span height count) #:transparent)

(define-match-expander IntervalNode*
  (lambda (stx)
    (syntax-case stx ()
      [(_ l r)
       #'(struct* IntervalNode ([left l] [right r]))])))

;; Top-level immutable rope structure holding base coordinate `origin` and root tree.
;; Represents intervals in absolute range [origin, origin + (tree-span root)).
;; An empty rope is normalized to origin = 0 and root = empty-tree.
(struct IntervalRope (origin root) #:transparent)

(define empty-tree (IntervalLeaf 0 *gap*))
(define empty-interval-rope (IntervalRope 0 empty-tree))

;; -----------------------------------------------------------------------------
;; Constructors & Predicates
;; -----------------------------------------------------------------------------

;; (->* () ((listof (cons/c (cons/c exact-integer? exact-integer?) any/c))) IntervalRope?)
(define (interval-rope [entries '()])
  (for/fold ([rope empty-interval-rope])
            ([entry (in-list entries)])
    (define range (car entry))
    (define value (cdr entry))
    (interval-rope-set rope (car range) (cdr range) value)))

;; (-> any/c boolean?)
(define (interval-rope? v)
  (IntervalRope? v))

;; (-> IntervalRope? boolean?)
(define (interval-rope-empty? rope)
  (zero? (interval-rope-count rope)))

;; (-> IntervalRope? exact-integer?)
(define (interval-rope-origin rope)
  (IntervalRope-origin rope))

;; (-> IntervalRope? exact-nonnegative-integer?)
(define (interval-rope-span rope)
  (tree-span (IntervalRope-root rope)))

;; (-> IntervalRope? exact-nonnegative-integer?)
(define (interval-rope-count rope)
  (tree-count (IntervalRope-root rope)))

;; -----------------------------------------------------------------------------
;; Tree Measurements & Node Construction
;; -----------------------------------------------------------------------------

(define (tree-span tree)
  (if (IntervalLeaf? tree)
      (IntervalLeaf-span tree)
      (IntervalNode-span tree)))

(define (tree-height tree)
  (if (IntervalLeaf? tree)
      (if (zero? (IntervalLeaf-span tree)) 0 1)
      (IntervalNode-height tree)))

(define (tree-count tree)
  (if (IntervalLeaf? tree)
      (if (or (zero? (IntervalLeaf-span tree))
              (eq? (IntervalLeaf-value tree) *gap*))
          0
          1)
      (IntervalNode-count tree)))

(define (tree-empty? tree)
  (zero? (tree-span tree)))

(define (make-node left right)
  (IntervalNode left right
                (+ (tree-span left) (tree-span right))
                (add1 (max (tree-height left) (tree-height right)))
                (+ (tree-count left) (tree-count right))))

(define (make-normalized-rope origin root)
  (if (or (tree-empty? root) (zero? (tree-count root)))
      empty-interval-rope
      (IntervalRope origin root)))

;; -----------------------------------------------------------------------------
;; AVL Rotations, Balancing & Splitting (Via common/avl-operations.rkt)
;; -----------------------------------------------------------------------------

(define-avl-operations
  #:node-match IntervalNode*
  #:make-node make-node
  #:height tree-height
  #:empty? tree-empty?
  #:tree-concat tree-concat-raw)

(define (tree-leftmost tree)
  (if (IntervalLeaf? tree)
      tree
      (tree-leftmost (IntervalNode-left tree))))

(define (tree-rightmost tree)
  (if (IntervalLeaf? tree)
      tree
      (tree-rightmost (IntervalNode-right tree))))

(define (pop-leftmost tree)
  (if (IntervalLeaf? tree)
      (values empty-tree tree)
      (let-values ([(rem edge) (pop-leftmost (IntervalNode-left tree))])
        (values (tree-concat-raw rem (IntervalNode-right tree)) edge))))

(define (pop-rightmost tree)
  (if (IntervalLeaf? tree)
      (values empty-tree tree)
      (let-values ([(rem edge) (pop-rightmost (IntervalNode-right tree))])
        (values (tree-concat-raw (IntervalNode-left tree) rem) edge))))

;; Concatenate two balanced trees, coalescing touching gap leaves at the seam.
(define (tree-concat left right)
  (cond
    [(tree-empty? left) right]
    [(tree-empty? right) left]
    [(and (eq? (IntervalLeaf-value (tree-rightmost left)) *gap*)
          (eq? (IntervalLeaf-value (tree-leftmost right)) *gap*))
     (define-values (prefix last-leaf) (pop-rightmost left))
     (define-values (suffix first-leaf) (pop-leftmost right))
     (define merged-gap
       (IntervalLeaf (+ (IntervalLeaf-span last-leaf)
                        (IntervalLeaf-span first-leaf))
                     *gap*))
     (tree-concat-raw (tree-concat-raw prefix merged-gap) suffix)]
    [else (tree-concat-raw left right)]))

(define (tree-split-at-offset tree offset)
  (define span (tree-span tree))
  (cond
    [(tree-empty? tree) (values empty-tree empty-tree)]
    [(<= offset 0) (values empty-tree tree)]
    [(>= offset span) (values tree empty-tree)]
    [(IntervalLeaf? tree)
     (define val (IntervalLeaf-value tree))
     (values (IntervalLeaf offset val)
             (IntervalLeaf (- span offset) val))]
    [else
     (match-define (IntervalNode* left right) tree)
     (define left-span (tree-span left))
     (cond
       [(= offset left-span) (values left right)]
       [(< offset left-span)
        (define-values (split-l split-r) (tree-split-at-offset left offset))
        (values split-l (tree-concat-raw split-r right))]
       [else
        (define-values (split-l split-r)
          (tree-split-at-offset right (- offset left-span)))
        (values (tree-concat-raw left split-l) split-r)])]))

;; -----------------------------------------------------------------------------
;; Validation & Error Reporting
;; -----------------------------------------------------------------------------

(define (valid-interval! who start end)
  (unless (exact-integer? start)
    (raise-argument-error who "exact-integer?" start))
  (unless (exact-integer? end)
    (raise-argument-error who "exact-integer?" end))
  (unless (< start end)
    (raise-arguments-error who "start is not strictly less than end"
                           "start" start
                           "end" end)))

(define (valid-query-interval! who start end)
  (unless (exact-integer? start)
    (raise-argument-error who "exact-integer?" start))
  (unless (exact-integer? end)
    (raise-argument-error who "exact-integer?" end))
  (unless (<= start end)
    (raise-arguments-error who "query interval start is greater than end"
                           "start" start
                           "end" end)))

(define (missing who default [key #f])
  (cond
    [(eq? default *absent*)
     (if key
         (error who "no mapping found\n  key: ~e" key)
         (error who "no mapping found"))]
    [(procedure? default)
     (default)]
    [else
     default]))

;; -----------------------------------------------------------------------------
;; Point & Range Queries
;; -----------------------------------------------------------------------------

;; (->* (IntervalRope? exact-integer?) (any/c)
;;      (values (or/c exact-integer? #f) (or/c exact-integer? #f) any/c))
(define (interval-rope-ref/bounds rope key [default *absent*])
  (unless (exact-integer? key)
    (raise-argument-error 'interval-rope-ref/bounds "exact-integer?" key))
  (match-define (IntervalRope origin root) rope)
  (let search ([tree root] [base origin])
    (cond
      [(or (tree-empty? tree) (< key base) (>= key (+ base (tree-span tree))))
       (values #f #f (missing 'interval-rope-ref/bounds default key))]
      [(IntervalLeaf? tree)
       (if (eq? (IntervalLeaf-value tree) *gap*)
           (values #f #f (missing 'interval-rope-ref/bounds default key))
           (values base (+ base (IntervalLeaf-span tree)) (IntervalLeaf-value tree)))]
      [else
       (define left (IntervalNode-left tree))
       (if (< key (+ base (tree-span left)))
           (search left base)
           (search (IntervalNode-right tree) (+ base (tree-span left))))])))

;; (->* (IntervalRope? exact-integer?) (any/c) any/c)
(define (interval-rope-ref rope key [default *absent*])
  (unless (exact-integer? key)
    (raise-argument-error 'interval-rope-ref "exact-integer?" key))
  (define-values (_s _e val) (interval-rope-ref/bounds rope key default))
  val)

;; Return all mapped values overlapping the coordinate range [start, end) in order.
;; When start = end = pos, selects strictly containing intervals (start < pos < end).
;; (-> IntervalRope? exact-integer? exact-integer? (listof any/c))
(define (interval-rope-overlap-values rope start end)
  (valid-query-interval! 'interval-rope-overlap-values start end)
  (match-define (IntervalRope origin root) rope)
  (let collect ([tree root] [base origin] [acc '()])
    (cond
      [(or (tree-empty? tree)
           (zero? (tree-count tree))
           (<= (+ base (tree-span tree)) start)
           (>= base end))
       acc]
      [(IntervalLeaf? tree)
       (if (eq? (IntervalLeaf-value tree) *gap*)
           acc
           (cons (IntervalLeaf-value tree) acc))]
      [else
       (define left (IntervalNode-left tree))
       (define right (IntervalNode-right tree))
       (define right-base (+ base (tree-span left)))
       (collect left base (collect right right-base acc))])))

;; -----------------------------------------------------------------------------
;; Pure Transformations
;; -----------------------------------------------------------------------------

;; Ensure the tree covers [start, end) by padding with *gap* leaves on the
;; left (adjusting origin) and/or right if the range extends beyond bounds.
;; Returns (values new-origin covered-tree).
(define (tree-cover root origin start end)
  (if (tree-empty? root)
      (values start (IntervalLeaf (- end start) *gap*))
      (let* ([new-origin (min origin start)]
             [left-pad (if (< start origin)
                           (IntervalLeaf (- origin start) *gap*)
                           empty-tree)]
             [current-end (+ origin (tree-span root))]
             [right-pad (if (> end current-end)
                            (IntervalLeaf (- end current-end) *gap*)
                            empty-tree)])
        (values new-origin
                (tree-concat (tree-concat left-pad root) right-pad)))))

(define (tree-replace root origin start end replacement)
  (define-values (new-origin covered) (tree-cover root origin start end))
  (define-values (left rest) (tree-split-at-offset covered (- start new-origin)))
  (define-values (_mid right) (tree-split-at-offset rest (- end start)))
  (values new-origin (tree-concat (tree-concat left replacement) right)))

;; (-> IntervalRope? exact-integer? exact-integer? any/c IntervalRope?)
(define (interval-rope-set rope start end value)
  (valid-interval! 'interval-rope-set start end)
  (match-define (IntervalRope origin root) rope)
  (define-values (new-origin new-root)
    (tree-replace root origin start end (IntervalLeaf (- end start) value)))
  (make-normalized-rope new-origin new-root))

;; (-> IntervalRope? exact-integer? exact-integer? IntervalRope?)
(define (interval-rope-remove rope start end)
  (valid-interval! 'interval-rope-remove start end)
  (match-define (IntervalRope origin root) rope)
  (if (or (tree-empty? root) (zero? (tree-count root)))
      rope
      (let* ([limit (+ origin (tree-span root))]
             [left (max start origin)]
             [right (min end limit)])
        (if (>= left right)
            rope
            (let-values ([(new-origin new-root)
                          (tree-replace root origin left right
                                        (IntervalLeaf (- right left) *gap*))])
              (make-normalized-rope new-origin new-root))))))

;; Shift all coordinates at or after `from` rightward by (- to from) (text insertion).
;; Uses a 'split strategy for interior insertions by injecting an unmapped *gap* leaf:
;; - O(1) if from <= origin (shifts origin directly)
;; - O(log N) if inside the tree:
;;   - #:interior 'split inserts a *gap* leaf of width (- to from)
;;   - #:interior 'extend keeps a strict interior insertion within the same mapped interval
;; - O(1) if from is past the tree end (no-op)
;; (->* (IntervalRope? exact-integer? exact-integer?) (#:interior (or/c 'split 'extend)) IntervalRope?)
(define (interval-rope-expand rope from to #:interior [interior 'split])
  (valid-interval! 'interval-rope-expand from to)
  (unless (memq interior '(split extend))
    (raise-argument-error 'interval-rope-expand "(or/c 'split 'extend)" interior))
  (match-define (IntervalRope origin root) rope)
  (if (or (tree-empty? root) (zero? (tree-count root)))
      rope
      (let ([shift (- to from)])
        (cond
          [(<= from origin)
           (IntervalRope (+ origin shift) root)]
          [(< from (+ origin (tree-span root)))
           (cond
             [(eq? interior 'extend)
              (define-values (range-start range-end value)
                (interval-rope-ref/bounds rope from *gap*))
              (define expanded (interval-rope-expand rope from to #:interior 'split))
              (if (and (not (eq? value *gap*))
                       (< range-start from range-end))
                  (interval-rope-set expanded range-start (+ range-end shift) value)
                  expanded)]
             [else
              (define-values (left right) (tree-split-at-offset root (- from origin)))
              (IntervalRope origin
                            (tree-concat (tree-concat left (IntervalLeaf shift *gap*))
                                         right))])]
          [else rope]))))

(define (interval-rope-expand/extend rope from to)
  (interval-rope-expand rope from to #:interior 'extend))

;; Concatenate two trees after a contraction/deletion, coalescing adjacent
;; mapped leaves at the seam if they share identical (eq?) user values.
;; (Adjacent *gap* leaves are handled by tree-concat).
(define (coalescing-concat left right)
  (cond
    [(tree-empty? left) right]
    [(tree-empty? right) left]
    [else
     (define a (tree-rightmost left))
     (define b (tree-leftmost right))
     (if (and (not (eq? (IntervalLeaf-value a) *gap*))
              (eq? (IntervalLeaf-value a) (IntervalLeaf-value b)))
         (let-values ([(prefix last-leaf) (pop-rightmost left)]
                      [(suffix first-leaf) (pop-leftmost right)])
           (tree-concat (tree-concat prefix
                                     (IntervalLeaf (+ (IntervalLeaf-span last-leaf)
                                                      (IntervalLeaf-span first-leaf))
                                                   (IntervalLeaf-value last-leaf)))
                        suffix))
         (tree-concat left right))]))

;; Delete coordinates in [from, to) and shift trailing coordinates leftward (text deletion).
;; Uses coalescing-concat to merge touching leaves if deletion brings identical values together:
;; - O(1) if to <= origin (shifts origin directly)
;; - O(log N) if inside the tree (slices out range and joins with coalescing-concat)
;; - O(1) if from is past the tree end (no-op)
;; (-> IntervalRope? exact-integer? exact-integer? IntervalRope?)
(define (interval-rope-contract rope from to)
  (valid-interval! 'interval-rope-contract from to)
  (match-define (IntervalRope origin root) rope)
  (if (or (tree-empty? root) (zero? (tree-count root)))
      rope
      (let ([shift (- to from)]
            [limit (+ origin (tree-span root))])
        (cond
          [(<= to origin)
           (IntervalRope (- origin shift) root)]
          [(< from limit)
           (define-values (left rest)
             (tree-split-at-offset root (max 0 (- from origin))))
           (define-values (_discard right)
             (tree-split-at-offset rest (- (min to limit) (max from origin))))
           (make-normalized-rope (min origin from) (coalescing-concat left right))]
          [else rope]))))

;; (->* (IntervalRope? exact-integer? exact-integer? (-> any/c any/c)) (any/c) IntervalRope?)
(define (interval-rope-update rope start end updater [default *absent*])
  (valid-interval! 'interval-rope-update start end)
  (match-define (IntervalRope origin root) rope)
  (define-values (new-origin covered) (tree-cover root origin start end))
  (define-values (left rest) (tree-split-at-offset covered (- start new-origin)))
  (define-values (middle right) (tree-split-at-offset rest (- end start)))
  (define updated-gap (delay (updater (missing 'interval-rope-update default))))
  (define (update-tree tree)
    (cond
      [(IntervalLeaf? tree)
       (IntervalLeaf (IntervalLeaf-span tree)
                     (if (eq? (IntervalLeaf-value tree) *gap*)
                         (force updated-gap)
                         (updater (IntervalLeaf-value tree))))]
      [else
       (define l (update-tree (IntervalNode-left tree)))
       (define r (update-tree (IntervalNode-right tree)))
       (IntervalNode l r (IntervalNode-span tree) (IntervalNode-height tree)
                     (+ (tree-count l) (tree-count r)))]))
  (make-normalized-rope new-origin
                        (tree-concat (tree-concat left (update-tree middle))
                                     right)))

;; (->* (IntervalRope? exact-integer? exact-integer? any/c) (any/c) IntervalRope?)
(define (interval-rope-cons rope start end value [default '()])
  (interval-rope-update rope start end (lambda (old) (cons value old)) default))

;; -----------------------------------------------------------------------------
;; Import & Export
;; -----------------------------------------------------------------------------

;; (-> IntervalRope? (listof (cons/c (cons/c exact-integer? exact-integer?) any/c)))
(define (interval-rope->list rope)
  (match-define (IntervalRope origin root) rope)
  (let collect ([tree root] [base origin] [acc '()])
    (cond
      [(or (tree-empty? tree) (zero? (tree-count tree))) acc]
      [(IntervalLeaf? tree)
       (if (eq? (IntervalLeaf-value tree) *gap*)
           acc
           (cons (cons (cons base (+ base (IntervalLeaf-span tree)))
                       (IntervalLeaf-value tree))
                 acc))]
      [else
       (define left (IntervalNode-left tree))
       (define right (IntervalNode-right tree))
       (define right-base (+ base (tree-span left)))
       (collect left base (collect right right-base acc))])))

;; (-> IntervalRope? (-> exact-integer? exact-integer? any/c any) void?)
(define (interval-rope-for-each rope proc)
  (match-define (IntervalRope origin root) rope)
  (let walk ([tree root] [base origin])
    (cond
      [(or (tree-empty? tree) (zero? (tree-count tree))) (void)]
      [(IntervalLeaf? tree)
       (unless (eq? (IntervalLeaf-value tree) *gap*)
         (proc base (+ base (IntervalLeaf-span tree)) (IntervalLeaf-value tree))
         (void))]
      [else
       (define left (IntervalNode-left tree))
       (define right (IntervalNode-right tree))
       (walk left base)
       (walk right (+ base (tree-span left)))]))
  (void))

;; Traverses mapped intervals on demand in O(log N) space using an explicit stack.
;;
;; Invariants:
;; - Values reside only in mapped IntervalLeaf nodes (tree-count > 0, non-*gap*).
;; - When the stack is non-empty, the top frame is always (cons leaf start-coordinate).
;; - Deeper frames are (cons pending-subtree base) for unvisited right subtrees with count > 0.
;; - Subtrees with zero mapped intervals are skipped in O(1) during descent.
;;
;; (-> IntervalRope? (sequence/c exact-integer? exact-integer? any/c))
(define (in-interval-rope rope)
  (unless (IntervalRope? rope)
    (raise-argument-error 'in-interval-rope "IntervalRope?" rope))
  (define origin (IntervalRope-origin rope))
  (define root (IntervalRope-root rope))

  (define (stack->values stack)
    (define entry (car stack))
    (define leaf (car entry))
    (define start (cdr entry))
    (values start (+ start (IntervalLeaf-span leaf)) (IntervalLeaf-value leaf)))

  (define (descend tree base stack)
    (cond
      [(IntervalLeaf? tree)
       (cons (cons tree base) stack)]
      [else
       (define left (IntervalNode-left tree))
       (define right (IntervalNode-right tree))
       (define left-count (tree-count left))
       (define right-count (tree-count right))
       (cond
         [(zero? left-count)
          (descend right (+ base (tree-span left)) stack)]
         [(zero? right-count)
          (descend left base stack)]
         [else
          (define right-base (+ base (tree-span left)))
          (descend left base (cons (cons right right-base) stack))])]))

  (define (advance-stack stack)
    (if (pair? stack)
        (let ([entry (car stack)])
          (descend (car entry) (cdr entry) (cdr stack)))
        '()))

  (define (next-stack stack)
    (advance-stack (cdr stack)))

  (make-do-sequence
    (lambda ()
      (values
        stack->values
        next-stack
        (if (zero? (tree-count root))
            '()
            (descend root origin '()))
        pair?
        #f
        #f))))

;; Construct an IntervalRope directly from sorted, disjoint start/end/id triples.
;; (-> (vectorof exact-integer?) (vectorof any/c) IntervalRope?)
(define (adopt-interval-snapshot entries decoded)
  (unless (vector? entries)
    (raise-argument-error 'adopt-interval-snapshot "vector?" entries))
  (unless (vector? decoded)
    (raise-argument-error 'adopt-interval-snapshot "vector?" decoded))
  (define total (vector-length entries))
  (unless (zero? (remainder total 3))
    (error 'adopt-interval-snapshot "snapshot entries vector length must be a multiple of 3"))

  (cond
    [(zero? total) empty-interval-rope]
    [else
     (define decoded-len (vector-length decoded))
     (define leaves (make-vector (* 2 (quotient total 3)) #f))
     (define used 0)
     (define origin (vector-ref entries 0))
     (define previous origin)
     (define (emit! width val)
       (when (positive? width)
         (vector-set! leaves used (IntervalLeaf width val))
         (set! used (add1 used))))

     (for ([offset (in-range 0 total 3)])
       (define start (vector-ref entries offset))
       (define end (vector-ref entries (+ offset 1)))
       (define id (vector-ref entries (+ offset 2)))
       (unless (and (exact-integer? start)
                    (exact-integer? end)
                    (<= previous start)
                    (< start end)
                    (exact-nonnegative-integer? id)
                    (< id decoded-len))
         (error 'adopt-interval-snapshot "invalid sorted disjoint snapshot entries"))
       (emit! (- start previous) *gap*)
       (emit! (- end start) (vector-ref decoded id))
       (set! previous end))

     (define (build-slice from to)
       (cond
         [(= from to) empty-tree]
         [(= (add1 from) to) (vector-ref leaves from)]
         [else
          (define mid (quotient (+ from to) 2))
          (make-node (build-slice from mid) (build-slice mid to))]))

     (make-normalized-rope origin (build-slice 0 used))]))

;; -----------------------------------------------------------------------------
;; Invariant Verification (Test Support)
;; -----------------------------------------------------------------------------

(define (check-interval-rope! rope)
  (match-define (IntervalRope origin root) rope)
  (define (check tree)
    (when (and tree (not (tree-empty? tree)))
      (unless (positive? (tree-span tree))
        (error 'rope-invariant "nonpositive span: ~a" (tree-span tree)))
      (when (IntervalNode? tree)
        (define left (IntervalNode-left tree))
        (define right (IntervalNode-right tree))
        (when (or (not left) (tree-empty? left)
                  (not right) (tree-empty? right))
          (error 'rope-invariant "internal node has empty child"))
        (unless (<= (abs (- (tree-height left) (tree-height right))) 1)
          (error 'rope-invariant "AVL balance invariant violated: heights ~a vs ~a"
                 (tree-height left) (tree-height right)))
        (when (and (eq? (IntervalLeaf-value (tree-rightmost left)) *gap*)
                   (eq? (IntervalLeaf-value (tree-leftmost right)) *gap*))
          (error 'rope-invariant "uncoalesced adjacent gaps across node boundary"))
        (unless (= (tree-span tree) (+ (tree-span left) (tree-span right)))
          (error 'rope-invariant "span sum mismatch: ~a vs (+ ~a ~a)"
                 (tree-span tree) (tree-span left) (tree-span right)))
        (unless (= (tree-height tree) (add1 (max (tree-height left) (tree-height right))))
          (error 'rope-invariant "height calculation mismatch"))
        (unless (= (tree-count tree) (+ (tree-count left) (tree-count right)))
          (error 'rope-invariant "count sum mismatch"))
        (check left)
        (check right))))

  (if (or (tree-empty? root) (zero? (tree-count root)))
      (unless (zero? origin)
        (error 'rope-invariant "empty rope has non-zero origin: ~a" origin))
      (check root))
  (void))

(module+ test-support
  (provide check-interval-rope!
           IntervalNode
           IntervalLeaf
           empty-tree
           IntervalRope))
