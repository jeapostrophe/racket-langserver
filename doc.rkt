#lang racket/base

(require "check-syntax.rkt"
         "monitor.rkt"
         "msg-io.rkt"
         "responses.rkt"
         "interfaces.rkt"
         "scheduler.rkt"
         racket/gui
         framework
         data/interval-map
         syntax-color/module-lexer
         syntax-color/racket-lexer
         json)

;; (struct/c Doc (is-a?/c text%) (is-a?/c build-trace%))
(struct Doc
  (text
   trace
   path
   during-batch-change?
   checked?)
  #:transparent #:mutable)

(define (send-diagnostics doc diag-lst)
  (display-message/flush (diagnostics-message (Doc-path doc) diag-lst)))

;; the only place where really run check-syntax
(define (doc-run-check-syntax doc)
  (define (task)
    (set-Doc-checked?! doc #f)
    (match-define (list new-trace diags)
      (report-time (check-syntax (Doc-path doc) (Doc-text doc) (Doc-trace doc))))
    (send-diagnostics doc diags)
    (set-Doc-trace! doc new-trace)

    (set-Doc-checked?! doc #t))

  (scheduler-push-task! (Doc-path doc) task))

(define (lazy-check-syntax doc)
  (when (not (Doc-during-batch-change? doc))
    (doc-run-check-syntax doc)))

(define (doc-checked? doc)
  (Doc-checked? doc))

(define (new-doc path text)
  (define doc-text (new racket:text%))
  (send doc-text insert text 0)
  (define doc (Doc doc-text #f path #f #f))
  (lazy-check-syntax doc)
  doc)

(define (doc-reset! doc new-text)
  (define doc-text (Doc-text doc))
  (define doc-trace (Doc-trace doc))

  (send doc-text erase)
  (send doc-trace reset)
  (send doc-text insert new-text 0)
  (lazy-check-syntax doc))

(define (doc-update! doc st-ln st-ch ed-ln ed-ch text)
  (define doc-text (Doc-text doc))
  (define doc-trace (Doc-trace doc))

  (define st-pos (doc-pos doc st-ln st-ch))
  (define end-pos (doc-pos doc ed-ln ed-ch))
  (define old-len (- end-pos st-pos))
  (define new-len (string-length text))
  (cond [(> new-len old-len) (send doc-trace expand end-pos (+ st-pos new-len))]
        [(< new-len old-len) (send doc-trace contract (+ st-pos new-len) end-pos)]
        [else #f])
  (send doc-text insert text st-pos end-pos)
  (lazy-check-syntax doc))

(define-syntax-rule (doc-batch-change doc expr ...)
  (let ()
    (set-Doc-during-batch-change?! doc #t)
    expr ...
    (set-Doc-during-batch-change?! doc #f)
    (lazy-check-syntax doc)))

(define (doc-pos doc line ch)
  (+ ch (send (Doc-text doc) paragraph-start-position line)))

(define (doc-line/ch doc pos)
  (define t (Doc-text doc))
  (define line (send t position-paragraph pos))
  (define line-begin (send t paragraph-start-position line))
  (define char (- pos line-begin))
  (values line char))

(define (doc-line-start-pos doc line)
  (send (Doc-text doc) paragraph-start-position line))

(define (doc-line-end-pos doc line)
  (send (Doc-text doc) paragraph-end-position line))

(define (doc-endpos doc)
  (send (Doc-text doc) last-position))

(define (doc-find-containing-paren doc pos)
  (define text (send (Doc-text doc) get-text))
  (define l (string-length text))
  (cond
    [(>= pos l) #f]
    [else
     (let loop ([i pos] [p 0])
       (cond
         [(< i 0) #f]
         [(or (char=? (string-ref text i) #\() (char=? (string-ref text i) #\[))
          (if (> p 0) (loop (- i 1) (- p 1)) i)]
         [(or (char=? (string-ref text i) #\)) (char=? (string-ref text i) #\]))
          (loop (- i 1) (+ p 1))]
         [else (loop (- i 1) p)]))]))

(define (get-symbols doc-text)
  (define text (send doc-text get-text))
  (define in (open-input-string text))
  (port-count-lines! in)
  (define lexer (get-lexer in))
  (define symbols (make-interval-map))
  (for ([lst (in-port (lexer-wrap lexer) in)]
        #:when (set-member? '(constant string symbol) (first (rest lst))))
    (match-define (list text type paren? start end) lst)
    (interval-map-set! symbols start end (list text type)))
  symbols)

;; Wrapper for in-port, returns a list or EOF.
(define ((lexer-wrap lexer) in)
  (define (eof-or-list txt type paren? start end)
    (if (eof-object? txt)
        eof
        (list txt type paren? start end)))
  (cond
    [(procedure? lexer)
     (define-values (txt type paren? start end)
       (lexer in))
     (eof-or-list txt type paren? start end)]
    [(cons? lexer)
     (define-values (txt type paren? start end backup mode)
       ((car lexer) in 0 (cdr lexer)))
     (set! lexer (cons (car lexer) mode))
     (eof-or-list txt type paren? start end)]))

;; Call module-lexer on an input port, then discard all
;; values except the lexer.
(define (get-lexer in)
  (match-define-values
    (_ _ _ _ _ _ lexer)
    (module-lexer in 0 #f))
  (cond
    [(procedure? lexer) lexer]
    [(cons? lexer) lexer]
    [(eq? lexer 'no-lang-line) racket-lexer]
    [(eq? lexer 'before-lang-line) racket-lexer]))

(define (doc-get-symbols doc)
  (get-symbols (Doc-text doc)))

;; formatting ;;

;; Shared path for all formatting requests
(define (format! this-doc st-ln st-ch ed-ln ed-ch #:on-type? [on-type? #f])
  (define doc-text (Doc-text this-doc))
  (define doc-trace (Doc-trace this-doc))

  (define indenter (send doc-trace get-indenter))
  (define start-pos (doc-pos this-doc st-ln st-ch))
  ;; Adjust for line endings (#92)
  (define end-pos (max start-pos (sub1 (doc-pos this-doc ed-ln ed-ch))))
  (define start-line (send doc-text position-paragraph start-pos))
  (define end-line (send doc-text position-paragraph end-pos))
  (define mut-doc-text
    (if (is-a? doc-text racket:text%)
        (let ([r-text (new racket:text%)])
          (send r-text insert (send doc-text get-text))
          r-text)
        (send doc-text copy-self)))
  (define skip-this-line? #f)
  (if (eq? indenter 'missing) (json-null)
      (let loop ([line start-line])
        (define line-start (send mut-doc-text paragraph-start-position line))
        (define line-end (send mut-doc-text paragraph-end-position line))
        (for ([i (range line-start (add1 line-end))])
          (when (and (char=? #\" (send mut-doc-text get-character i))
                     (not (char=? #\\ (send mut-doc-text get-character (sub1 i)))))
            (set! skip-this-line? (not skip-this-line?))))
        (if (> line end-line)
            null
            (append (filter-map
                     identity
                     ;; NOTE: The order is important here.
                     ;; `remove-trailing-space!` deletes content relative to the initial document
                     ;; position. If we were to instead call `indent-line!` first and then
                     ;; `remove-trailing-space!` second, the remove step could result in
                     ;; losing user entered code.
                     (list (remove-trailing-space! mut-doc-text skip-this-line? line)
                           (indent-line! mut-doc-text indenter line #:on-type? on-type?)))
                    (loop (add1 line)))))))

;; Returns a TextEdit, or #f if the line is a part of multiple-line string
(define (remove-trailing-space! doc-text in-string? line)
  (define line-start (send doc-text paragraph-start-position line))
  (define line-end (send doc-text paragraph-end-position line))
  (define line-text (send doc-text get-text line-start line-end))
  (cond
    [(not in-string?)
     (define from (string-length (string-trim line-text #px"\\s+" #:left? #f)))
     (define to (string-length line-text))
     (send doc-text delete (+ line-start from) (+ line-start to))
     (TextEdit #:range (Range #:start (Pos #:line line #:char from)
                              #:end   (Pos #:line line #:char to))
               #:newText "")]
    [else #f]))

;; Returns a TextEdit, or #f if the line is already correct.
(define (indent-line! doc-text indenter line #:on-type? [on-type? #f])
  (define line-start (send doc-text paragraph-start-position line))
  (define line-end (send doc-text paragraph-end-position line))
  (define line-text (send doc-text get-text line-start line-end))
  (define line-length (string-length line-text))
  (define current-spaces
    (let loop ([i 0])
      (cond [(= i line-length) i]
            [(char=? (string-ref line-text i) #\space) (loop (add1 i))]
            [else i])))
  (define desired-spaces
    (if indenter
        (or (indenter doc-text line-start)
            (send doc-text compute-racket-amount-to-indent line-start))
        (send doc-text compute-racket-amount-to-indent line-start)))
  (cond
    [(not (number? desired-spaces)) #f]
    [(= current-spaces desired-spaces) #f]
    [(and (not on-type?) (= line-length 0)) #f]
    [(< current-spaces desired-spaces)
     ;; Insert spaces
     (define insert-count (- desired-spaces current-spaces))
     (define new-text (make-string insert-count #\space))
     (define pos (Pos #:line line #:char 0))
     (send doc-text insert new-text line-start 'same)
     (TextEdit #:range (Range #:start pos #:end pos)
               #:newText new-text)]
    [else
     ;; Delete spaces
     (define span (- current-spaces desired-spaces))
     (send doc-text delete line-start (+ line-start span))
     (TextEdit #:range (Range #:start (Pos #:line line #:char 0)
                              #:end   (Pos #:line line #:char span))
               #:newText "")]))

(provide Doc-trace
         new-doc
         doc-checked?
         doc-update!
         doc-reset!
         doc-batch-change
         doc-pos
         doc-endpos
         doc-line/ch
         doc-line-start-pos
         doc-line-end-pos
         doc-find-containing-paren
         doc-get-symbols
         format!)
