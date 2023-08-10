#lang racket/base

(require "check-syntax.rkt"
         "msg-io.rkt"
         "responses.rkt"
         "interfaces.rkt"
         "scheduler.rkt"
         "editor.rkt"
         "path-util.rkt"
         "doc-trace.rkt"
         "struct.rkt"
         racket/match
         racket/class
         racket/set
         racket/list
         racket/string
         racket/bool
         data/interval-map
         syntax-color/module-lexer
         syntax-color/racket-lexer
         json
         drracket/check-syntax
         syntax/modread)

(struct Doc
  (text
   trace
   uri
   during-batch-change?
   checked?)
  #:transparent #:mutable)

(define (send-diagnostics doc diag-lst)
  (display-message/flush (diagnostics-message (Doc-uri doc) diag-lst)))

;; the only place where really run check-syntax
(define (doc-run-check-syntax doc)
  (define (task)
    (set-Doc-checked?! doc #f)
    (match-define (list new-trace diags)
      (check-syntax (uri->path (Doc-uri doc)) (Doc-text doc) (Doc-trace doc)))
    (send-diagnostics doc diags)
    (set-Doc-trace! doc new-trace)

    (set-Doc-checked?! doc #t))

  (scheduler-push-task! (Doc-uri doc) task))

(define (lazy-check-syntax doc)
  (when (not (Doc-during-batch-change? doc))
    (doc-run-check-syntax doc)))

(define (doc-checked? doc)
  (Doc-checked? doc))

(define (new-doc uri text)
  (define doc-text (new lsp-editor%))
  (send doc-text insert text 0)
  ;; the init trace should not be #f
  (define doc-trace (new build-trace% [src (uri->path uri)] [doc-text doc-text] [indenter #f]))
  (define doc (Doc doc-text doc-trace uri #f #f))
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

  ;; try reuse old information as the check-syntax can fail
  ;; and return the old build-trace% object
  (cond [(> new-len old-len) (send doc-trace expand end-pos (+ st-pos new-len))]
        [(< new-len old-len) (send doc-trace contract (+ st-pos new-len) end-pos)])
  (send doc-text replace text st-pos end-pos)
  (lazy-check-syntax doc))

(define-syntax-rule (doc-batch-change doc expr ...)
  (let ()
    (set-Doc-during-batch-change?! doc #t)
    expr ...
    (set-Doc-during-batch-change?! doc #f)
    (lazy-check-syntax doc)))

(define (doc-pos doc line ch)
  (send (Doc-text doc) line/char->pos line ch))

(define (doc-line/ch doc pos)
  (match-define (list line char) (send (Doc-text doc) pos->line/char pos))
  (values line char))

(define (doc-line-start-pos doc line)
  (send (Doc-text doc) line-start-pos line))

(define (doc-line-end-pos doc line)
  (send (Doc-text doc) line-end-pos line))

(define (doc-endpos doc)
  (send (Doc-text doc) end-pos))

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
    [(eq? lexer 'before-lang-line) racket-lexer]
    [else racket-lexer]))

(define (doc-get-symbols doc)
  (get-symbols (Doc-text doc)))

;; definition BEG ;;

(define (get-def path doc-text id)
  (define collector
    (new (class (annotations-mixin object%)
           (define defs (make-hash))
           (define/public (get id) (hash-ref defs id #f))
           (define/override (syncheck:add-definition-target source-obj start end id mods)
             (hash-set! defs id (cons start end)))
           (super-new))))
  (define-values (src-dir _file _dir?)
    (split-path path))
  (define in (open-input-string (send doc-text get-text)))

  (define ns (make-base-namespace))
  (define-values (add-syntax done)
    (make-traversal ns src-dir))
  (parameterize ([current-annotations collector]
                 [current-namespace ns]
                 [current-load-relative-directory src-dir])
    (define stx (expand (with-module-reading-parameterization
                          (λ () (read-syntax path in)))))
    (add-syntax stx))
  (send collector get id))

(define (get-definition-by-id path id)
  (define doc-text (new lsp-editor%))
  (send doc-text load-file path)
  (match-define (cons start end) (get-def path doc-text id))
  (match-define (list st-ln st-ch) (send doc-text pos->line/char start))
  (match-define (list ed-ln ed-ch) (send doc-text pos->line/char end))
  (make-Range #:start (make-Position #:line st-ln #:character st-ch)
              #:end (make-Position #:line ed-ln #:character ed-ch)))

;; definition END ;;

;; formatting ;;

;; Shared path for all formatting requests
(define (format! this-doc st-ln st-ch ed-ln ed-ch
                 #:on-type? [on-type? #f]
                 #:formatting-options opts)
  (define doc-text (Doc-text this-doc))
  (define doc-trace (Doc-trace this-doc))

  (define indenter (send doc-trace get-indenter))
  (define start-pos (doc-pos this-doc st-ln st-ch))
  ;; Adjust for line endings (#92)
  (define end-pos (max start-pos (sub1 (doc-pos this-doc ed-ln ed-ch))))
  (define start-line (send doc-text at-line start-pos))
  (define end-line (send doc-text at-line end-pos))

  (define mut-doc-text (send doc-text copy))
  ;; replace \t with spaces at line `(sub1 start-line)`
  ;; as we cannot make `compute-racket-amount-to-indent`
  ;; to respect the given tab size
  (replace-tab! mut-doc-text
                (max 0 (sub1 start-line))
                (FormattingOptions-tab-size opts))

  (define indenter-wp (indenter-wrapper indenter mut-doc-text on-type?))
  (define skip-this-line? #f)

  (if (eq? indenter 'missing) (json-null)
      (let loop ([line start-line])
        (define line-start (send mut-doc-text line-start-pos line))
        (define line-end (send mut-doc-text line-end-pos line))
        (for ([i (range line-start (add1 line-end))])
          (when (and (char=? #\" (send mut-doc-text get-char i))
                     (not (char=? #\\ (send mut-doc-text get-char (sub1 i)))))
            (set! skip-this-line? (not skip-this-line?))))
        (if (> line end-line)
            null
            (append (filter-map
                     values
                     ;; NOTE: The order is important here.
                     ;; `remove-trailing-space!` deletes content relative to the initial document
                     ;; position. If we were to instead call `indent-line!` first and then
                     ;; `remove-trailing-space!` second, the remove step could result in
                     ;; losing user entered code.
                     (list (if (false? (FormattingOptions-trim-trailing-whitespace opts))
                               #f
                               (remove-trailing-space! mut-doc-text skip-this-line? line))
                           (indent-line! mut-doc-text indenter-wp line)))
                    (loop (add1 line)))))))

(define (replace-tab! doc-text line tabsize)
  (define old-line (send doc-text get-line line))
  (define spaces (make-string tabsize #\space))
  (define new-line-str (string-replace old-line "\t" spaces))
  (send doc-text replace-in-line
        new-line-str
        line 0 (string-length old-line)))

(define (indenter-wrapper indenter doc-text on-type?)
  (λ (line)
    (cond [(and (not on-type?)
                (= (send doc-text line-start-pos line)
                   (send doc-text line-end-pos line)))
           #f]
          [else
           (define line-start (send doc-text line-start-pos line))
           (if indenter
               (or (indenter doc-text line-start)
                   (send doc-text compute-racket-amount-to-indent line-start))
               (send doc-text compute-racket-amount-to-indent line-start))])))

;; Returns a TextEdit, or #f if the line is a part of multiple-line string
(define (remove-trailing-space! doc-text in-string? line)
  (define line-text (send doc-text get-line line))
  (cond
    [(not in-string?)
     (define from (string-length (string-trim line-text #px"\\s+" #:left? #f)))
     (define to (string-length line-text))
     (send doc-text replace-in-line "" line from to)
     (TextEdit #:range (Range #:start (Pos #:line line #:char from)
                              #:end   (Pos #:line line #:char to))
               #:newText "")]
    [else #f]))

(define (extract-indent-string content)
  (define len
    (or (for/first ([(c i) (in-indexed content)]
                    #:when (not (char-whitespace? c)))
          i)
        (string-length content)))
  (substring content 0 len))

;; Returns a TextEdit, or #f if the line is already correct.
(define (indent-line! doc-text indenter line)
  (define content (send doc-text get-line line))
  (define old-indent-string (extract-indent-string content))
  (define expect-indent (indenter line))
  (define really-indent (string-length old-indent-string))
  (define has-tab? (string-contains? old-indent-string "\t"))

  (cond [(false? expect-indent) #f]
        [(and (= expect-indent really-indent) (not has-tab?)) #f]
        [else
         (define new-text (make-string expect-indent #\space))
         (send doc-text replace-in-line new-text line 0 really-indent)
         (TextEdit #:range (Range #:start (Pos #:line line #:char 0)
                                  #:end (Pos #:line line #:char really-indent))
                   #:newText new-text)]))

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
         get-definition-by-id
         format!)
