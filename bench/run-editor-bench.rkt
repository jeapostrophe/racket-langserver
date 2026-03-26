#lang racket/base

(require racket/class
         racket/cmdline
         racket/runtime-path)

(define-runtime-path repo-root "..")

(struct Scenario
  (name iterations thunk)
  #:transparent)

(define backend-spec "auto")
(define fixture-line-count 2000)
(define fixture-line-width 40)
(define iteration-scale 1)

(define (parse-exact-positive who raw)
  (define value (string->number raw))
  (unless (and (exact-integer? value)
               (positive? value))
    (error who "expected a positive exact integer, got ~a" raw))
  value)

(define (resolve-backend-path spec)
  (define alias-path
    (cond [(string-ci=? spec "auto")
           (build-path repo-root "doclib" "editor.rkt")]
          [(string-ci=? spec "legacy")
           (build-path repo-root "doclib" "textbuffer" "editor-legacy.rkt")]
          [(string-ci=? spec "rope")
           (build-path repo-root "doclib" "textbuffer" "editor-rope.rkt")]
          [else #f]))
  (define candidate-path
    (or alias-path
        (let ([raw-path (string->path spec)])
          (if (relative-path? raw-path)
              (build-path repo-root raw-path)
              raw-path))))
  (define normalized-path (simplify-path candidate-path))
  (unless (file-exists? normalized-path)
    (error 'run-editor-bench
           "editor module does not exist: ~a"
           normalized-path))
  normalized-path)

(define (make-fixture-text line-count line-width)
  (apply string-append
         (for/list ([line (in-range line-count)])
           (string-append
             (build-string line-width
                           (lambda (column)
                             (if (= (modulo (+ line column 1) 31) 0)
                                 #\return
                                 (integer->char
                                   (+ (char->integer #\a)
                                      (modulo (+ line column) 26))))))
             "\n"))))

(define (load-editor-class module-path)
  (dynamic-require module-path 'lsp-editor%))

(define (scaled-iterations base)
  (* iteration-scale base))

(define (print-result scenario cpu-ms real-ms gc-ms)
  (printf "~a\n" (Scenario-name scenario))
  (printf "iterations: ~a\n" (Scenario-iterations scenario))
  (printf "cpu: ~a ms" cpu-ms)
  (printf " real: ~a ms" (real->decimal-string real-ms 3))
  (printf " gc: ~a ms" gc-ms)
  (printf " avg-real: ~a us\n"
          (real->decimal-string
            (* 1000.0 (/ real-ms (Scenario-iterations scenario)))
            3)))

(define (run-scenario scenario)
  ((Scenario-thunk scenario))
  (collect-garbage)
  (define start-ms (current-inexact-milliseconds))
  (define-values (_results cpu-ms _ignored-real-ms gc-ms)
    (time-apply
      (lambda ()
        (for ([_ (in-range (Scenario-iterations scenario))])
          ((Scenario-thunk scenario))))
      '()))
  (define real-ms (- (current-inexact-milliseconds) start-ms))
  (print-result scenario cpu-ms real-ms gc-ms))

(command-line
  #:program "run-editor-bench.rkt"
  #:once-each
  [("--backend") spec
   "Backend alias or module path: auto, legacy, rope, or a relative/absolute module path"
   (set! backend-spec spec)]
  [("--lines") count
   "Fixture line count"
   (set! fixture-line-count (parse-exact-positive '--lines count))]
  [("--line-width") width
   "Fixture line width before each terminating newline"
   (set! fixture-line-width (parse-exact-positive '--line-width width))]
  [("--scale") scale
   "Multiply each scenario's iteration count by this positive integer"
   (set! iteration-scale (parse-exact-positive '--scale scale))])

(module+ main
  (define module-path (resolve-backend-path backend-spec))
  (define editor% (load-editor-class module-path))
  (define source-text (make-fixture-text fixture-line-count fixture-line-width))

  (define (make-editor)
    (define editor (new editor%))
    (send editor insert source-text 0)
    editor)

  (define base-editor (make-editor))
  (define middle-line (quotient fixture-line-count 2))
  (define middle-column (quotient fixture-line-width 2))
  (define top-insert-line (min 5 (sub1 fixture-line-count)))
  (define top-insert-column (min 10 fixture-line-width))
  (define multiline-start-line (max 0 (sub1 middle-line)))
  (define multiline-end-line (min (sub1 fixture-line-count) (+ multiline-start-line 3)))
  (define multiline-start-column (min 5 fixture-line-width))
  (define multiline-end-column (min 10 fixture-line-width))
  (define multiline-replacement "hello\nworld\n")

  (define middle-pos
    (send base-editor line/char->pos middle-line middle-column))
  (define get-text-range-start-pos
    (send base-editor line/char->pos multiline-start-line multiline-start-column))
  (define get-text-range-end-pos
    (send base-editor line/char->pos multiline-end-line multiline-end-column))
  (define top-insert-pos
    (send base-editor line/char->pos top-insert-line top-insert-column))
  (define multiline-start-pos
    (send base-editor line/char->pos multiline-start-line multiline-start-column))
  (define multiline-end-pos
    (send base-editor line/char->pos multiline-end-line multiline-end-column))

  (define replace-editor (make-editor))
  (define replace-original-text
    (send replace-editor get-text middle-pos (add1 middle-pos)))

  (define insert-editor (make-editor))

  (define multiline-editor (make-editor))
  (define multiline-original-text
    (send multiline-editor get-text multiline-start-pos multiline-end-pos))
  (define multiline-replacement-end-pos
    (+ multiline-start-pos (string-length multiline-replacement)))

  (define typing-editor (make-editor))
  (define typing-burst-length 50)

  (define scenarios
    (list
      (Scenario
        "build editor from source text"
        (scaled-iterations 50)
        (lambda ()
          (void (make-editor))))
      (Scenario
        "copy editor snapshot"
        (scaled-iterations 100)
        (lambda ()
          (void (send base-editor copy))))
      (Scenario
        "line/char->pos middle"
        (scaled-iterations 10000)
        (lambda ()
          (void (send base-editor line/char->pos middle-line middle-column))))
      (Scenario
        "pos->line/char middle"
        (scaled-iterations 10000)
        (lambda ()
          (void (send base-editor pos->line/char middle-pos))))
      (Scenario
        "get-char middle"
        (scaled-iterations 100000)
        (lambda ()
          (void (send base-editor get-char middle-pos))))
      (Scenario
        "get-text full document"
        (scaled-iterations 200)
        (lambda ()
          (void (send base-editor get-text))))
      (Scenario
        "get-text middle range"
        (scaled-iterations 5000)
        (lambda ()
          (void (send base-editor get-text
                      get-text-range-start-pos
                      get-text-range-end-pos))))
      (Scenario
        "single top-of-file insert with restore"
        (scaled-iterations 2000)
        (lambda ()
          (send insert-editor replace "x" top-insert-pos top-insert-pos)
          (send insert-editor delete top-insert-pos (add1 top-insert-pos))))
      (Scenario
        "single middle replacement with restore"
        (scaled-iterations 2000)
        (lambda ()
          (send replace-editor replace "x" middle-pos (add1 middle-pos))
          (send replace-editor replace replace-original-text middle-pos (add1 middle-pos))))
      (Scenario
        "small multiline replace with restore"
        (scaled-iterations 1000)
        (lambda ()
          (send multiline-editor replace
                multiline-replacement
                multiline-start-pos
                multiline-end-pos)
          (send multiline-editor replace
                multiline-original-text
                multiline-start-pos
                multiline-replacement-end-pos)))
      (Scenario
        "typing burst of 50 inserts with restore"
        (scaled-iterations 100)
        (lambda ()
          (for ([_ (in-range typing-burst-length)])
            (send typing-editor replace "x" top-insert-pos top-insert-pos))
          (send typing-editor delete
                top-insert-pos
                (+ top-insert-pos typing-burst-length))))))

  (printf "backend: ~a\n" backend-spec)
  (printf "module: ~a\n" module-path)
  (printf "fixture: lines=~a width=~a chars=~a\n\n"
          fixture-line-count
          fixture-line-width
          (string-length source-text))

  (for ([scenario (in-list scenarios)])
    (run-scenario scenario)
    (newline)))

