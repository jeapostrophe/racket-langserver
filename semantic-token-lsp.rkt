#lang racket/base

(require racket/contract
         racket/list
         "interfaces.rkt")

(define (token-type-encoding token)
  (index-of semantic-token-types (SemanticToken-type token)))

(define (token-modifier-encoding token)
  (define indexes
    (indexes-where semantic-token-modifiers
                   (λ (modifier)
                     (member modifier (SemanticToken-modifiers token)))))
  ;; build a bit flag of the modifiers of `token`.
  ;;
  ;; equivalent to C family pseudocode
  ;;
  ;; uint32_t flag = 0
  ;; for index in indexes:
  ;;   flag = flag | (1 << index)
  ;; return flag
  ;;
  ;; But the integer bit width is ignored here, because
  ;; semantic-token-modifiers is currently very small.
  (for/sum ([index indexes])
    (expt 2 index)))

;; encode `token` using relative encoding
;;
;; each token is encoded as five integers (copied from lsp specification 3.17):
;; * deltaLine: token line number, relative to the start of the previous token
;; * deltaStart: token start character, relative to the start of the previous token
;;               (relative to 0 or the previous token’s start if they are on the same line)
;; * length: the length of the token.
;; * tokenType: will be looked up in SemanticTokensLegend.tokenTypes.
;;              We currently ask that tokenType < 65536.
;; * tokenModifiers: each set bit will be looked up in SemanticTokensLegend.tokenModifiers
;;
;; for the first token, its previous token is defined as a zero length fake token which
;; has line number 0 and character position 0.
(define (token-encoding abs-pos->pos token prev-pos)
  (define pos (abs-pos->pos (SemanticToken-start token)))
  (define prev (abs-pos->pos prev-pos))
  (define line (Pos-line pos))
  (define char (Pos-char pos))
  (define prev-line (Pos-line prev))
  (define prev-char (Pos-char prev))
  (define delta-line (- line prev-line))
  (define delta-start
    (if (= line prev-line)
        (- char prev-char)
        char))
  (define len (- (SemanticToken-end token) (SemanticToken-start token)))
  (define type (token-type-encoding token))
  (define modifier (token-modifier-encoding token))
  (values delta-line delta-start len type modifier))

(define/contract (encode-semantic-tokens abs-pos->pos tokens)
  (-> (-> exact-nonnegative-integer? Pos?)
      (listof SemanticToken?)
      (listof exact-nonnegative-integer?))
  (for/fold ([result '()]
             [prev-pos 0]
             #:result (flatten (reverse result)))
            ([token tokens])
    (define-values (delta-line delta-start len type modifier)
      (token-encoding abs-pos->pos token prev-pos))
    (values (cons (list delta-line delta-start len type modifier) result)
            (SemanticToken-start token))))

(provide encode-semantic-tokens)

