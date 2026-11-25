#lang racket/base

;; Immutable AVL interval rope with relative spans and unmapped gap sentinels.
;; Aligned in structure, rotation helpers, and immutability with
;; doclib/textbuffer/textbuffer-rope.rkt.
;;
;; Design:
;; 1. Immutability:
;;    IntervalRope is a pure value. All transformation functions (set, remove,
;;    expand, contract, update) return a new IntervalRope without mutating
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
         racket/match
         (for-syntax racket/base))

(provide empty-interval-rope
         interval-rope
         interval-rope-from-sorted
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
         interval-rope-contract
         interval-rope-update
         ;; Export & iteration
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
(struct IntervalLeaf (span value) #:transparent #:authentic)

;; A Node balances two non-empty subtrees.
;; - `span`: relative width of the subtree (+ (tree-span left) (tree-span right))
;; - `height`: 1 + (max (tree-height left) (tree-height right))
;; - `count`: number of mapped (non-*gap*) leaves in this subtree
;; Nodes store no absolute coordinates.
(struct IntervalNode (left right span height count) #:transparent #:authentic)

(define-match-expander IntervalNode*
  (lambda (stx)
    (syntax-case stx ()
      [(_ l r)
       #'(struct* IntervalNode ([left l] [right r]))])))

;; Top-level immutable rope structure holding base coordinate `origin` and root tree.
;; Represents intervals in absolute range [origin, origin + (tree-span root)).
;; An empty rope is normalized to origin = 0 and root = empty-tree, and any
;; non-empty rope has mapped (non-*gap*) leftmost and rightmost boundary leaves.
(struct IntervalRope (origin root) #:transparent #:authentic)

(define empty-tree (IntervalLeaf 0 *gap*))
(define empty-interval-rope (IntervalRope 0 empty-tree))

;; -----------------------------------------------------------------------------
;; Constructors & Predicates
;; -----------------------------------------------------------------------------

(define (build-balanced-leaves leaves from to)
  (cond
    [(= from to) empty-tree]
    [(= (add1 from) to) (vector-ref leaves from)]
    [else
     (define mid (quotient (+ from to) 2))
     (make-node (build-balanced-leaves leaves from mid)
                (build-balanced-leaves leaves mid to))]))

;; (->* () ((listof (cons/c (cons/c exact-integer? exact-integer?) any/c))) IntervalRope?)
(define (interval-rope [entries '()])
  (unless (list? entries)
    (raise-argument-error 'interval-rope "list?" entries))
  (for/fold ([rope empty-interval-rope])
            ([entry (in-list entries)])
    (define-values (start end val) (parse-entry 'interval-rope entry))
    (set-core rope start end val)))

;; (-> (listof (cons/c (cons/c exact-integer? exact-integer?) any/c)) IntervalRope?)
(define (interval-rope-from-sorted entries)
  (unless (list? entries)
    (raise-argument-error 'interval-rope-from-sorted "list?" entries))
  (cond
    [(null? entries) empty-interval-rope]
    [else
     (define leaves (make-vector (* 2 (length entries)) #f))
     (define-values (first-start _prev-end used)
       (for/fold ([first-start #f] [prev-end #f] [used 0])
                 ([entry (in-list entries)])
         (define-values (start end val) (parse-entry 'interval-rope-from-sorted entry))
         (when (and prev-end (< start prev-end))
           (raise-arguments-error 'interval-rope-from-sorted
                                  "entries are not sorted and non-overlapping"
                                  "previous end" prev-end
                                  "start" start))
         (define next-used
           (cond
             [(and prev-end (> start prev-end))
              (vector-set! leaves used (IntervalLeaf (- start prev-end) *gap*))
              (add1 used)]
             [else used]))
         (vector-set! leaves next-used (IntervalLeaf (- end start) val))
         (values (or first-start start) end (add1 next-used))))
     (IntervalRope first-start (build-balanced-leaves leaves 0 used))]))

;; (-> any/c boolean?)
(define (interval-rope? v)
  (IntervalRope? v))

;; (-> IntervalRope? boolean?)
(define (interval-rope-empty? rope)
  (check-rope! 'interval-rope-empty? rope)
  (zero? (interval-rope-count rope)))

;; (-> IntervalRope? exact-integer?)
(define (interval-rope-origin rope)
  (check-rope! 'interval-rope-origin rope)
  (IntervalRope-origin rope))

;; (-> IntervalRope? exact-nonnegative-integer?)
(define (interval-rope-span rope)
  (check-rope! 'interval-rope-span rope)
  (tree-span (IntervalRope-root rope)))

;; (-> IntervalRope? exact-nonnegative-integer?)
(define (interval-rope-count rope)
  (check-rope! 'interval-rope-count rope)
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
  (cond
    [(or (tree-empty? root) (zero? (tree-count root)))
     empty-interval-rope]
    [else
     (define-values (trimmed-origin after-left)
       (if (eq? (IntervalLeaf-value (tree-leftmost root)) *gap*)
           (let-values ([(rest left-gap) (pop-leftmost root)])
             (values (+ origin (IntervalLeaf-span left-gap)) rest))
           (values origin root)))
     (define after-right
       (if (eq? (IntervalLeaf-value (tree-rightmost after-left)) *gap*)
           (let-values ([(rest _right-gap) (pop-rightmost after-left)])
             rest)
           after-left))
     (IntervalRope trimmed-origin after-right)]))

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

(define (check-rope! who v)
  (unless (IntervalRope? v)
    (raise-argument-error who "IntervalRope?" v)))

(define (parse-entry who entry)
  (unless (and (pair? entry) (pair? (car entry)))
    (raise-argument-error who "(cons/c (cons/c exact-integer? exact-integer?) any/c)" entry))
  (define range (car entry))
  (define start (car range))
  (define end (cdr range))
  (valid-interval! who start end)
  (values start end (cdr entry)))

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

(define (ref/bounds-core who rope key default)
  (check-rope! who rope)
  (unless (exact-integer? key)
    (raise-argument-error who "exact-integer?" key))
  (match-define (IntervalRope origin root) rope)
  (let search ([tree root] [base origin])
    (cond
      [(or (tree-empty? tree) (< key base) (>= key (+ base (tree-span tree))))
       (values #f #f (missing who default key))]
      [(IntervalLeaf? tree)
       (if (eq? (IntervalLeaf-value tree) *gap*)
           (values #f #f (missing who default key))
           (values base (+ base (IntervalLeaf-span tree)) (IntervalLeaf-value tree)))]
      [else
       (define left (IntervalNode-left tree))
       (if (< key (+ base (tree-span left)))
           (search left base)
           (search (IntervalNode-right tree) (+ base (tree-span left))))])))

;; (->* (IntervalRope? exact-integer?) (any/c)
;;      (values (or/c exact-integer? #f) (or/c exact-integer? #f) any/c))
(define (interval-rope-ref/bounds rope key [default *absent*])
  (ref/bounds-core 'interval-rope-ref/bounds rope key default))

;; (->* (IntervalRope? exact-integer?) (any/c) any/c)
(define (interval-rope-ref rope key [default *absent*])
  (define-values (_s _e val) (ref/bounds-core 'interval-rope-ref rope key default))
  val)

;; Return all mapped values overlapping the coordinate range [start, end) in order.
;; When start = end = pos (cursor-only query), selects strictly containing
;; intervals (base < pos < base + span, excluding base = pos) to preserve
;; cursor-only code-action containment semantics.
;; (-> IntervalRope? exact-integer? exact-integer? (listof any/c))
(define (interval-rope-overlap-values rope start end)
  (check-rope! 'interval-rope-overlap-values rope)
  (valid-query-interval! 'interval-rope-overlap-values start end)
  (match-define (IntervalRope origin root) rope)
  (let collect ([tree root] [base origin] [acc '()])
    (cond
      [(or (zero? (tree-count tree))
           (<= (+ base (tree-span tree)) start)
           (>= base end))
       acc]
      [(IntervalLeaf? tree)
       (cons (IntervalLeaf-value tree) acc)]
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

(define (set-core rope start end value)
  (match-define (IntervalRope origin root) rope)
  (define span (- end start))
  (cond
    [(tree-empty? root)
     (IntervalRope start (IntervalLeaf span value))]
    [(>= start (+ origin (tree-span root)))
     (define gap-width (- start (+ origin (tree-span root))))
     (define leaf (IntervalLeaf span value))
     (IntervalRope origin
                   (if (positive? gap-width)
                       (tree-concat-raw (tree-concat-raw root (IntervalLeaf gap-width *gap*))
                                        leaf)
                       (tree-concat-raw root leaf)))]
    [(<= end origin)
     (define gap-width (- origin end))
     (define leaf (IntervalLeaf span value))
     (IntervalRope start
                   (if (positive? gap-width)
                       (tree-concat-raw leaf
                                        (tree-concat-raw (IntervalLeaf gap-width *gap*) root))
                       (tree-concat-raw leaf root)))]
    [else
     (define-values (new-origin new-root)
       (tree-replace root origin start end (IntervalLeaf span value)))
     (make-normalized-rope new-origin new-root)]))

;; (-> IntervalRope? exact-integer? exact-integer? any/c IntervalRope?)
(define (interval-rope-set rope start end value)
  (check-rope! 'interval-rope-set rope)
  (valid-interval! 'interval-rope-set start end)
  (set-core rope start end value))

;; (-> IntervalRope? exact-integer? exact-integer? IntervalRope?)
(define (interval-rope-remove rope start end)
  (check-rope! 'interval-rope-remove rope)
  (valid-interval! 'interval-rope-remove start end)
  (match-define (IntervalRope origin root) rope)
  (if (tree-empty? root)
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

;; Insert a `*gap*` of width `shift` at `offset` (0 < offset < (tree-span tree)),
;; or widen a strictly containing leaf when `extend?` is true.
(define (tree-expand tree offset shift extend?)
  (cond
    [(IntervalLeaf? tree)
     (define val (IntervalLeaf-value tree))
     (if (or extend? (eq? val *gap*))
         (IntervalLeaf (+ (IntervalLeaf-span tree) shift) val)
         (make-node (make-node (IntervalLeaf offset val)
                               (IntervalLeaf shift *gap*))
                    (IntervalLeaf (- (IntervalLeaf-span tree) offset) val)))]
    [else
     (match-define (IntervalNode* left right) tree)
     (define left-span (tree-span left))
     (cond
       [(< offset left-span)
        (tree-concat-raw (tree-expand left offset shift extend?) right)]
       [(> offset left-span)
        (tree-concat-raw left (tree-expand right (- offset left-span) shift extend?))]
       [else
        (tree-concat (tree-concat left (IntervalLeaf shift *gap*)) right)])]))

;; Shift all coordinates at or after `from` rightward by (- to from) (text insertion).
;; Uses a 'split strategy for interior insertions by injecting an unmapped *gap* leaf:
;; - O(1) if from <= origin (shifts origin directly)
;; - O(log N) if inside the tree:
;;   - #:interior 'split inserts a *gap* leaf of width (- to from) (widening gaps in place)
;;   - #:interior 'extend widens a strictly containing leaf in a single O(log N) descent
;; - O(1) if from is past the tree end (no-op)
;; (->* (IntervalRope? exact-integer? exact-integer?) (#:interior (or/c 'split 'extend)) IntervalRope?)
(define (interval-rope-expand rope from to #:interior [interior 'split])
  (check-rope! 'interval-rope-expand rope)
  (valid-interval! 'interval-rope-expand from to)
  (unless (memq interior '(split extend))
    (raise-argument-error 'interval-rope-expand "(or/c 'split 'extend)" interior))
  (match-define (IntervalRope origin root) rope)
  (if (tree-empty? root)
      rope
      (let ([shift (- to from)])
        (cond
          [(<= from origin)
           (IntervalRope (+ origin shift) root)]
          [(< from (+ origin (tree-span root)))
           (IntervalRope origin
                         (tree-expand root (- from origin) shift (eq? interior 'extend)))]
          [else rope]))))

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
           (tree-concat-raw
             (tree-concat-raw prefix
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
  (check-rope! 'interval-rope-contract rope)
  (valid-interval! 'interval-rope-contract from to)
  (match-define (IntervalRope origin root) rope)
  (if (tree-empty? root)
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
  (check-rope! 'interval-rope-update rope)
  (valid-interval! 'interval-rope-update start end)
  (unless (procedure? updater)
    (raise-argument-error 'interval-rope-update "procedure?" updater))
  (match-define (IntervalRope origin root) rope)
  (define-values (new-origin covered) (tree-cover root origin start end))
  (define-values (left rest) (tree-split-at-offset covered (- start new-origin)))
  (define-values (middle right) (tree-split-at-offset rest (- end start)))
  (define gap-val *absent*)
  (define (get-gap-val)
    (when (eq? gap-val *absent*)
      (set! gap-val (updater (missing 'interval-rope-update default))))
    gap-val)
  (define (update-tree tree)
    (cond
      [(IntervalLeaf? tree)
       (IntervalLeaf (IntervalLeaf-span tree)
                     (if (eq? (IntervalLeaf-value tree) *gap*)
                         (get-gap-val)
                         (updater (IntervalLeaf-value tree))))]
      [else
       (define l (update-tree (IntervalNode-left tree)))
       (define r (update-tree (IntervalNode-right tree)))
       (IntervalNode l r (IntervalNode-span tree) (IntervalNode-height tree)
                     (+ (tree-count l) (tree-count r)))]))
  (make-normalized-rope new-origin
                        (tree-concat (tree-concat left (update-tree middle))
                                     right)))

;; -----------------------------------------------------------------------------
;; Export & Iteration
;; -----------------------------------------------------------------------------

;; (-> IntervalRope? (listof (cons/c (cons/c exact-integer? exact-integer?) any/c)))
(define (interval-rope->list rope)
  (check-rope! 'interval-rope->list rope)
  (match-define (IntervalRope origin root) rope)
  (let collect ([tree root] [base origin] [acc '()])
    (cond
      [(zero? (tree-count tree)) acc]
      [(IntervalLeaf? tree)
       (cons (cons (cons base (+ base (IntervalLeaf-span tree)))
                   (IntervalLeaf-value tree))
             acc)]
      [else
       (define left (IntervalNode-left tree))
       (define right (IntervalNode-right tree))
       (define right-base (+ base (tree-span left)))
       (collect left base (collect right right-base acc))])))

;; (-> IntervalRope? (-> exact-integer? exact-integer? any/c any) void?)
(define (interval-rope-for-each rope proc)
  (check-rope! 'interval-rope-for-each rope)
  (unless (procedure? proc)
    (raise-argument-error 'interval-rope-for-each "procedure?" proc))
  (match-define (IntervalRope origin root) rope)
  (let walk ([tree root] [base origin])
    (cond
      [(zero? (tree-count tree)) (void)]
      [(IntervalLeaf? tree)
       (proc base (+ base (IntervalLeaf-span tree)) (IntervalLeaf-value tree))
       (void)]
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
  (check-rope! 'in-interval-rope rope)
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

  (cond
    [(or (tree-empty? root) (zero? (tree-count root)))
     (unless (tree-empty? root)
       (error 'rope-invariant "zero-count rope has non-empty root"))
     (unless (zero? origin)
       (error 'rope-invariant "empty rope has non-zero origin: ~a" origin))]
    [else
     (check root)
     (when (eq? (IntervalLeaf-value (tree-leftmost root)) *gap*)
       (error 'rope-invariant "non-empty rope has leading gap"))
     (when (eq? (IntervalLeaf-value (tree-rightmost root)) *gap*)
       (error 'rope-invariant "non-empty rope has trailing gap"))])
  (void))

(module+ test-support
  (provide check-interval-rope!
           IntervalNode
           IntervalLeaf
           empty-tree
           IntervalRope))
