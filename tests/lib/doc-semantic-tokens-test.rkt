#lang racket/base

(module+ test
  (require "../silent-logger.rkt"
           rackunit
           racket/list
           racket/match
           "../../common/interfaces.rkt"
           "../../doclib/doc.rkt"
           "../../doclib/internal-types.rkt"
           "../../lsp/semantic-token-lsp.rkt")

  (test-case "Semantic token viewport ranges can extend past the end of the document"
    (define text "#; (ignored)")
    (define d (make-doc "file:///viewport.rkt" text))
    (define bounded (doc-range-tokens d (Range (Pos 0 0) (Pos 0 (string-length text)))))
    (check-not-equal? bounded '())
    (check-equal? (doc-range-tokens d (Range (Pos 0 0) (Pos 3 0))) bounded)
    (check-equal? (doc-range-tokens d (Range (Pos 0 0) (Pos 0 100))) bounded)
    (check-equal? (doc-range-tokens d (Range (Pos 3 0) (Pos 4 0))) '()))

  (test-case "Semantic tokens count UTF-16 code units after and within astral characters"
    (define d (make-doc "file:///utf16-tokens.rkt" "\U0001F600 #; (\U0001F600)"))
    (define tokens (doc-range-tokens d (Range (Pos 0 0) (Pos 0 10))))
    (check-equal? (take (encode-semantic-tokens (lambda (pos) (doc-abs-pos->pos d pos)) tokens) 3)
                  '(0 3 7)))

  (test-case "Range tokens (Semantic Tokens)"
    (define d (make-doc "file:///test.rkt" "#lang racket\n(define x 1)"))
    (define range (Range (Pos 0 0) (Pos 1 11)))
    (check-true (empty? (doc-range-tokens d range)) "tokens should be empty before doc-expand!")
    (check-true (doc-expand! d))
    (define after-expand (doc-range-tokens d range))
    (check-false (empty? after-expand) "tokens should exist after doc-expand!")
    (check-true (andmap SemanticToken? after-expand)))

  (test-case "Range tokens include sexp comment semantic tokens"
    (define text "#lang racket\n#; (define x 1)\n(+ 1 2)")
    (define d (make-doc "file:///test.rkt" text))
    (define comment-range (first (regexp-match-positions #px"#; \\(define x 1\\)" text)))
    (define tokens (doc-range-tokens d (Range (Pos 0 0) (Pos 2 7))))
    (define comment-token
      (findf (lambda (token) (eq? (SemanticToken-type token) SemanticTokenType-comment)) tokens))
    (check-true (SemanticToken? comment-token))
    (check-equal? (SemanticToken-start comment-token) (car comment-range))
    (check-equal? (SemanticToken-end comment-token) (cdr comment-range)))

  (test-case "Range tokens split multi-line sexp comment semantic tokens"
    (define text "#lang racket\n#;\n(define x 1)\n(+ 1 2)")
    (define d (make-doc "file:///test.rkt" text))
    (define tokens (doc-range-tokens d (Range (Pos 0 0) (Pos 3 7))))
    (define comment-ranges
      (for/list ([token (in-list tokens)]
                 #:when (eq? (SemanticToken-type token) SemanticTokenType-comment))
        (cons (SemanticToken-start token) (SemanticToken-end token))))
    (check-equal? comment-ranges
                  (list (first (regexp-match-positions #px"#;" text))
                        (first (regexp-match-positions #px"\\(define x 1\\)" text)))))

  (test-case "Range tokens remove stale trace tokens inside current sexp comments"
    (define d (make-doc "file:///test.rkt" "#lang racket\n(define x 1)\nx\n"))
    (check-true (doc-expand! d))
    (doc-apply-edit! d (Range (Pos 1 0) (Pos 1 0)) "#; ")

    (define updated-text (doc-get-text d))
    (match-define (cons comment-start comment-end)
      (first (regexp-match-positions #px"#; \\(define x 1\\)" updated-text)))
    (define tokens (doc-range-tokens d (Range (Pos 0 0) (Pos 3 0))))
    (define (token-intersects-comment? token)
      (char-range-intersect? (SemanticToken-start token)
                             (SemanticToken-end token)
                             comment-start
                             comment-end))
    (define (comment-token? token)
      (eq? (SemanticToken-type token) SemanticTokenType-comment))

    (define comment-token
      (findf (lambda (token)
               (and (comment-token? token)
                    (= (SemanticToken-start token) comment-start)
                    (= (SemanticToken-end token) comment-end)))
             tokens))
    (define non-comment-tokens (filter-not comment-token? tokens))
    (check-true (SemanticToken? comment-token))
    (check-false (ormap token-intersects-comment? non-comment-tokens))
    (check-not-false
      (findf (lambda (token)
               (and (not (comment-token? token))
                    (not (token-intersects-comment? token))))
             tokens))
    (check-true
      (for/and ([left (in-list tokens)]
                [right (in-list (rest tokens))])
        (<= (SemanticToken-start left) (SemanticToken-start right)))))

  (test-case "Semantic tokens count each CRLF as two characters"
    (define d
      (make-doc "file:///tmp/doc-crlf-tokens-test.rkt"
                "#lang racket\r\n(define x 1)\r\n#; x (+ x x)\r\n"))
    (doc-expand! d)
    (define tokens (doc-range-tokens d (Range (Pos 2 0) (Pos 3 0))))
    (check-equal? (encode-semantic-tokens (lambda (pos) (doc-abs-pos->pos d pos)) tokens)
                  '(2 0 4 5 0
                    0 6 1 1 0
                    0 2 1 0 0
                    0 2 1 0 0))))
