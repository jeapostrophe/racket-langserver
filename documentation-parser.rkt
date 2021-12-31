#lang racket/base
(require
  racket/match
  racket/string
  racket/format
  net/url-string
  html-parsing
  racket/function
  "docs-helpers.rkt")


;; ------------------------------------------
;; Cursor (aka html node tree navigator) is implemented with the Zipper data structure
;; The idea of its use is borrowed from a similar project: https://github.com/dyoo/wescheme-docs/blob/master/tree-cursor.rkt
;; Zipper is described in detail in a paper "Functional Pearl. The Zipper" by Gerard Huet

(struct cursor (selected-node lefts parent rights) #:transparent)
(define (make-cursor tree-node) (cursor tree-node '() #f '()))

(define (cursor-can-go-down? current-cursor)
  (match current-cursor [(cursor (list fst-child _ ...) _ _ _) #t] [_ #f]))
(define (cursor-can-go-up? current-cursor)
  (match current-cursor [(cursor _ _ parent _) #:when parent #t] [_ #f]))
(define (cursor-can-go-left? current-cursor)
  (match current-cursor [(cursor _ (list left-sibling _ ...) _ _) #t] [_ #f]))
(define (cursor-can-go-right? current-cursor)
  (match current-cursor [(cursor _ _ _ (list right-sibling _ ...)) #t] [_ #f]))

(define (cursor-go-down current-cursor)
  (match current-cursor
    [(cursor (list fst-child others ...) _ _ _)
     (cursor fst-child '() current-cursor others)]
    [_ (error "Cursor can't move down!")]))
(define (cursor-go-up current-cursor)
  (match current-cursor
    [(cursor selected-node lefts parent rights)
     (cursor (append (reverse lefts) (cons selected-node rights))
             (cursor-lefts parent) (cursor-parent parent) (cursor-rights parent))]
    [_ (error "Cursor can't move up!")]))
(define (cursor-go-up-until-true cursor predicate?)
  (cond
    [(predicate? (cursor-selected-node cursor)) cursor]
    [(cursor-can-go-up? cursor) (cursor-go-up-until-true (cursor-go-up cursor) predicate?)]
    [else #f]))
(define (cursor-go-left current-cursor)
  (match current-cursor
    [(cursor selected-node (list fst-left-sibling others ...) parent rights)
     (cursor fst-left-sibling others parent (cons selected-node rights))]
    [_ (error "Cursor can't move left!")]))
(define (cursor-go-right current-cursor)
  (match current-cursor
    [(cursor selected-node lefts parent (list fst-right-sibling others ...))
     (cursor fst-right-sibling (cons selected-node lefts) parent others)]
    [_ (error "Cursor can't move right!")]))
(define (cursor-go-to-next-sibling-or-uncle-node cursor doc-is-nested?)
  (define (is-outside-of-blockquote? cursor) (is-blockquote-leftindent? (cursor-selected-node cursor)))
  (cond
    [(cursor-can-go-right? cursor) (cursor-go-right cursor)]
    [(cursor-can-go-up? cursor)
     (define parent-cursor (cursor-go-up cursor))
     (cond
       ;; If currently parsed doc was `nested`, don't go outside of it - stop parsing here
       [(and doc-is-nested? (is-outside-of-blockquote? parent-cursor)) #f]
       [else (cursor-go-to-next-sibling-or-uncle-node parent-cursor doc-is-nested?)])]
    [else #f]))


;; ------------------------------------------
;; This functions are responsible for finding html nodes that constitute the selected element's documentation

(define (find-doc-beginning-and-take-cursor doc-xexp anchor-name)
  (define (find-node predicate? cursor)
    (cond
      [(predicate? (cursor-selected-node cursor)) cursor]
      [(cursor-can-go-down? cursor) (find-node predicate? (cursor-go-down cursor))]
      [(cursor-can-go-right? cursor) (find-node predicate? (cursor-go-right cursor))]
      [(cursor-can-go-up? cursor)
       (let loop ([cursor cursor])
         (cond
           [(cursor-can-go-right? cursor) (find-node predicate? (cursor-go-right cursor))]
           [(cursor-can-go-up? cursor) (loop (cursor-go-up cursor))]
           [else #f]
           ))]
      [else #f]))
  (define (find-doc-beginning cursor)
    (or
     (cursor-go-up-until-true cursor
                              (match-lambda [`(div (@ (class "SIntrapara")) ,_ ...) #t] [_ #f]))
     ;; Just in case someone for some peculiar reason forgot to include <div class="SIntrapara"> in the documentation
     (cursor-go-up-until-true cursor
                              (match-lambda [`(blockquote (@ (class "SVInsetFlow")) ,_ ...) #t] [_ #f]))))
  (define maybe-cursor (find-node
                        (λ (x) (equal? x (list 'name anchor-name)))
                        (make-cursor doc-xexp)))
  (and maybe-cursor (find-doc-beginning maybe-cursor)))

(define (selected-node-contains-documentation-boundary? cursor doc-is-nested?)
  (define (find-boundary-node predicate? tree)
    (cond
      ;; If the selected docs weren't nested themselves, then we collect nested docs without checking for their boundaries
      [(and (not doc-is-nested?) (is-blockquote-leftindent? tree)) #f]
      ;; if there is a list, then we want (probably?) take it as a whole, no matter what boundaries it may have
      [(tag-name? tree 'ul) #f]
      [(predicate? tree) tree]
      [(list? tree) (ormap (λ (x) (find-boundary-node predicate? x)) tree)]
      [else #f]))
  (find-boundary-node
   (match-lambda
     ;; beginning of docs for the next function, method, struct, or whatever
     [`(div (@ (class "RBackgroundLabelInner"))
            (p ,(or "class" "constructor" "interface" "method" "mixin" "parameter" "procedure" "signature" "struct" "syntax" "value"))) #t]
     ;; end of a doc list ;; e.g. <div class="navsetbottom">
     [`(@ (class "navsetbottom")) #t]
     ;; end of a module ;; e.g. <h5 x-source-module="..." ...>
     [`(@ (x-source-module ,_) ,_ ...) #t]
     [_ #f])
   (cursor-selected-node cursor)))

(define (collect-docs cursor doc-is-nested? [is-inside-boundary-node? #f])
  (cond [(not cursor) '()]
        ;; we gather doc elements sequentially until we meet documentation for another function
        [(selected-node-contains-documentation-boundary? cursor doc-is-nested?)
         ;; hack: the boundary node can be a paragraph <p> that still contains some elements that belong to our doc,
         ;; so we try to look inside just in case, but only once. That should be enough.
         (define node (cursor-selected-node cursor))
         (if (and (not is-inside-boundary-node?) (list? node) (> (length node) 1))
             (list (car node) ; tag name
                   (collect-docs (cursor-go-right (cursor-go-down cursor)) doc-is-nested? #t))
             '())]
        [else
         (define next-node-cursor (cursor-go-to-next-sibling-or-uncle-node cursor doc-is-nested?))
         (cons (cursor-selected-node cursor)
               (collect-docs next-node-cursor doc-is-nested? is-inside-boundary-node?))]))


;; ------------------------------------------
;; Conversion of HTML into Markdown GFM

(define (tag? mb-tag)
  (match mb-tag [(list tag-name _ ...) #:when (not (eq? '@ tag-name)) #t] [_ #f])) ; ignore attributes "('@ ...)"
(define (tag-name? tag name)
  (match tag [(list (== name)  _ ...) #t] [_ #f]))
(define (get-tag-attribute tag attr-name)
  (match tag
    [(list _ ... (list '@ _ ... (list (== attr-name) attr-value) _ ...) _ ...) attr-value] [_ #f]))
(define (tag-attribute? tag attr-name attr-value)
  (equal? attr-value (get-tag-attribute tag attr-name)))
(define (is-blockquote-leftindent? tree-node)
  (match tree-node [`(blockquote (@ (class "leftindent") ,_ ...) ,_ ...) #t] [_ #f]))

;; the function tries its best to convert html (in the form of SXML/xexp emitted by 'html-parsing' lib) to Markdown string
(define (html->markdown html-tag html-file-path)
  (define code-block-depth 0)
  (define list-element-depth 0)
  (define reference-links-table (make-hash))

  (define (recursive-convert tag [parent #f])
    (define inside-code-block? (> code-block-depth 0))
    (define inside-list-element? (> list-element-depth 0))

    (define (convert-tag-contents) (string-join (map (λ (child-tag) (recursive-convert child-tag tag)) tag) ""))
    (define (should-be-in-fenced-code-block? tag) (or (tag-attribute? tag 'class "SCodeFlow") (and (tag-name? tag 'table) (not inside-code-block?))))
    (define (should-be-emphasized? tag) (ormap (λ (x) (tag-attribute? tag 'class x)) '("RktVar" "RktVal" )))
    (define (ignore? tag) (ormap (λ (x) (tag-attribute? tag 'class x)) '("refcolumn" "RBackgroundLabelInner")))
    (define (escape-markdown str)
      (if inside-code-block? str ; in markdown everything inside code blocks is escaped by default
          (regexp-replace*
           #rx"[\n<>]" str
           (match-lambda
             ;; Otherwise text like "#<void>" would be rendered just as "#", because Md eats everything that looks like an html tag.
             ;; Also could be prepended with a slash ('\>') or replaced with an html entity ('&lt;'),
             ;; but the former doesn't work in Atom's Md parser and both don't look very well if the editor doesn't support Markdown.
             ["<" "❮"] [">" "❯"]
             ["\n" " "] [s s]))))

    ;; normally markdown supports html entities but only outside of code blocks
    (define (convert-if-tag-is-html-entity tag)
      (match tag ['(& nbsp) " "] ['(& rarr) "->"] ['(& rsquo) "'"] ['(& ldquo) "\""] ['(& rdquo) "\""] ['(& ndash) "-"] [_ #f]))

    (cond
      [(string? tag) (escape-markdown tag)]
      [(convert-if-tag-is-html-entity tag) => identity]
      [(or (not (tag? tag)) (ignore? tag)) ""] ; garbage or just tags that should be ignored for esthetic reasons
      [(tag-name? tag 'br) "  \n"] ; two spaces with a newline allow line splitting inside emphases (*...*)
      ;;
      [(tag-attribute? tag 'class "SHistory") (~a "\n\n*" (convert-tag-contents) "*")] ; changelog paragraph
      [(tag-name? tag 'tr) (~a "\n" (convert-tag-contents))]
      [(and (tag-name? tag 'p) (not inside-code-block?)) (~a (if inside-list-element? " " "\n\n") (convert-tag-contents))]
      [(and (tag-name? tag 'ul)) (~a (convert-tag-contents) "\n\n")]
      [(and (should-be-emphasized? tag) (not inside-code-block?)) (~a "*" (string-trim (convert-tag-contents)) "*")]

      ;; <blockquote class="leftindent"> usually contains nested docs; we show them indented as list elements for visibility
      ;; each list level is indented by 4 spaces
      [(or (tag-name? tag 'li)
           (and (is-blockquote-leftindent? tag) (not inside-code-block?)
                (match parent [`(,_ (@ ,_) (blockquote ,_ ...)) #f] [_ #t]))) ; don't indent blockquote if it's only one
       (set! list-element-depth (add1 list-element-depth))
       (define contents (string-trim (convert-tag-contents)))
       (set! list-element-depth (sub1 list-element-depth))
       (if (non-empty-string? contents)
           (~a "\n-   "
               (string-replace contents "\n" "\n    ") ; indent each line
               (if (tag-name? tag 'li) "" "\n"))
           "")]

      [(should-be-in-fenced-code-block? tag) ; mainly actual code blocks but also includes <table>s
       (set! code-block-depth (add1 code-block-depth))
       ;; get rid of unnecessary whitespaces
       (define codeblock-contents (string-trim (string-replace (convert-tag-contents) "\n\n" "\n")))
       (set! code-block-depth (sub1 code-block-depth))
       (~a "\n\n```\n" codeblock-contents "\n```\n\n")]

      [(or (and (tag-name? tag 'a) (not inside-code-block?)) (tag-name? tag 'img)) ; links (<a href=...>) or images (<img src=...>)
       ;; we replace relative URLs with absolute ones referencing online documentation (because offline links don't work in VSCode)
       ;; same for images with 'src' attribute
       (define is-img? (tag-name? tag 'img))
       (define mb-link-attr (get-tag-attribute tag (if is-img? 'src 'href)))
       (define maybe-url (if mb-link-attr
                             (make-proper-url-for-online-documentation mb-link-attr html-file-path) #f))
       (cond
         [(not maybe-url) (convert-tag-contents)] ; empty href -> no link
         ;; Image: ![ImageName](URL)
         [is-img?
          (define img (~a "![" (or (get-tag-attribute tag 'alt) "HereWasImage") "](" maybe-url ")"))
          (if inside-code-block? (~a "\n```\n" img "\n```\n") img)] ; temporarily close a code block as images can't be shown inside them
         ;; Reference link: [LinkName] ... [LinkName]: URL
         [else
          (define contents (convert-tag-contents))
          (hash-set! reference-links-table contents maybe-url)
          (~a "[" contents "]")])]

      [else (convert-tag-contents)]))

  ;; some cosmetic additions: combine consecutive emphasized regions and newlines for readability (in case the text editor doesn't support markdown)
  (define generated-markdown (string-replace (recursive-convert html-tag) "**" ""))
  (define prettified-markdown (string-trim (string-replace generated-markdown #px"\n{3,}" "\n\n")))
  (define reference-links (hash-map reference-links-table (λ (link-name link-url) (~a "[" link-name "]: " link-url))))
  (~a prettified-markdown "\n\n" (string-join reference-links "\n")))


;; ------------------------------------------
;; ------------------------------------------

;; `docs-uri` example: file:///C:/Program%20Files/Racket/doc/reference/define.html#(form._((lib._racket%2Fprivate%2Fbase..rkt)._define))
(define (extract-documentation docs-uri include-signature?)
  (define url (string->url docs-uri))
  (define doc-element-name (url-fragment url))
  (define html-file-path (string-join (map path/param-path (url-path url)) "/")) ; used for resolving URLs in the documentation

  (define maybe-html-file
    (with-handlers ([exn:fail:filesystem? (λ _ #f)])
      (open-input-file (url->path url))))

  (cond [(and maybe-html-file doc-element-name)
         (define doc-xexp (html->xexp maybe-html-file)) ; parse the html file with `html-parsing` lib
         (close-input-port maybe-html-file)

         ;; So how that html docs `extracting` stuff works.
         ;; At first, we find the first element (i.e. HTML node) of the documentation for the function that we've selected.
         ;; Almost always (99.99%) it's `<div class=SIntrapara>` that contains `<a name=`doc-element-name`>` inside.
         ;; And it also has the function signature.
         ;; After that, we start to collect all consecutive elements until we meet a boundary node.
         ;; The boundary node is an HTML tag that begins documentation for some other element (next function, method, etc).
         (define cursor-at-signature (find-doc-beginning-and-take-cursor doc-xexp doc-element-name))
         (cond [cursor-at-signature
                ;; Docs can be sort of nested in each other.
                ;; If `div` with signature is initially located inside <blockquote>, the documentation is counted as `nested`.
                ;; For example: https://docs.racket-lang.org/reference/require.html#(form._((lib._racket%2Fprivate%2Fbase..rkt)._require))
                ;; There, `only-in` is nested in `require`.
                ;; The distinction is important for detection of boundary nodes.
                ;; If we parse some nested doc, we want to stop before the next one.
                ;; But if the selected docs aren't nested, we want to also get every nested doc inside (if any), ignoring their boundary nodes.
                (define doc-is-nested? (cursor-go-up-until-true cursor-at-signature is-blockquote-leftindent?))

                (define cursor-after-signature (cursor-go-to-next-sibling-or-uncle-node cursor-at-signature doc-is-nested?))
                (define doc-without-signature-html (collect-docs cursor-after-signature doc-is-nested?))
                (define doc-without-signature-markdown (html->markdown doc-without-signature-html html-file-path))

                (~a (if include-signature?
                        (~a (html->markdown (cursor-selected-node cursor-at-signature) html-file-path) "\n---\n") "")
                    doc-without-signature-markdown)]
               [else #f])]
        [else #f]))

(define weak-cache (make-weak-hash))
(define (extract-documentation-for-selected-element docs-uri #:include-signature? include-signature?)
  (hash-ref! weak-cache docs-uri (thunk (extract-documentation docs-uri include-signature?))))


(provide extract-documentation-for-selected-element)
