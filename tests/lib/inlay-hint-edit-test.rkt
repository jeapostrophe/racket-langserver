#lang racket/base

;; What an edit does to hints that were read before it.

(module+ test
  (require rackunit
           (only-in "../../common/interfaces.rkt"
                    CharRange
                    CharRange-start
                    CharRange-end
                    InlayHintKind-Parameter)
           (only-in "../../doclib/inlay-hint-source.rkt"
                    Inlay-Hint-Anchor
                    Inlay-Hint-Anchor-pos
                    Inlay-Hint-Group
                    Inlay-Hint-Group-sources
                    Inlay-Hint-Group-anchors))

  (require/expose "../../doclib/service/inlay-hint.rkt"
                  (drop-disturbed-groups
                    expand-groups
                    contract-groups))

  ;; One hint drawn at 12, read from the form spanning [10, 20).
  (define (group-at [source (CharRange 10 20)] #:also [also '()])
    (Inlay-Hint-Group (cons source also)
                      (list (Inlay-Hint-Anchor 12 InlayHintKind-Parameter "x " "field x"))))

  (define (summarize groups)
    (for/list ([group (in-list groups)])
      (list (for/list ([range (in-list (Inlay-Hint-Group-sources group))])
              (list (CharRange-start range) (CharRange-end range)))
            (map Inlay-Hint-Anchor-pos (Inlay-Hint-Group-anchors group)))))

  (define one (list (group-at)))

  (test-case
    "an insert before a hint moves it and the form it was read from"
    (check-equal? (summarize (expand-groups one 0 5))
                  '((((15 25)) (17))))
    ;; text typed just before the form goes before all of it
    (check-equal? (summarize (expand-groups one 10 13))
                  '((((13 23)) (15)))))

  (test-case
    "an insert after a hint leaves it alone"
    (check-equal? (summarize (expand-groups one 20 25))
                  '((((10 20)) (12))))
    (check-equal? (summarize (expand-groups one 30 31))
                  '((((10 20)) (12)))))

  (test-case
    "a delete elsewhere moves the hints it did not touch"
    (check-equal? (summarize (contract-groups one 0 5))
                  '((((5 15)) (7))))
    (check-equal? (summarize (contract-groups one 5 10))
                  '((((5 15)) (7))))
    (check-equal? (summarize (contract-groups one 20 30))
                  '((((10 20)) (12)))))

  (test-case
    "a delete that takes the whole form away leaves nothing to describe"
    (check-equal? (contract-groups one 0 100) '()))

  ;; `drop-disturbed-groups` is given the text the edit replaced, an insert
  ;; being the empty interval.

  (test-case
    "an edit that reaches into the form drops its hints"
    (check-equal? (drop-disturbed-groups one 15 15) '())
    (check-equal? (drop-disturbed-groups one 11 11) '())
    (check-equal? (drop-disturbed-groups one 15 16) '())
    (check-equal? (drop-disturbed-groups one 5 15) '())
    (check-equal? (drop-disturbed-groups one 10 20) '()))

  (test-case
    "an edit that stops at either end of the form leaves its hints alone"
    ;; an insert there goes next to the form rather than inside it, and a
    ;; replace there rewrites the text beside the form, not the form
    (check-equal? (summarize (drop-disturbed-groups one 10 10))
                  '((((10 20)) (12))))
    (check-equal? (summarize (drop-disturbed-groups one 20 20))
                  '((((10 20)) (12))))
    (check-equal? (summarize (drop-disturbed-groups one 0 10))
                  '((((10 20)) (12))))
    (check-equal? (summarize (drop-disturbed-groups one 20 30))
                  '((((10 20)) (12)))))

  (test-case
    "a rewrite is judged by the text it replaced, not by the length it changed"
    ;; rewriting [10, 20) with fifteen characters is the expand below, which
    ;; is also what typing five characters after the form would be
    (check-equal? (summarize (expand-groups one 20 25))
                  '((((10 20)) (12))))
    (check-equal? (drop-disturbed-groups one 10 20) '()))

  (test-case
    "a hint read from more than one form goes when any of them changes"
    ;; a constructor call names its arguments after a struct declared
    ;; elsewhere: editing that declaration changes what they mean
    (define with-declaration (list (group-at #:also (list (CharRange 100 130)))))
    (check-equal? (drop-disturbed-groups with-declaration 110 111) '())
    (check-equal? (drop-disturbed-groups with-declaration 100 130) '())
    (check-equal? (summarize (expand-groups with-declaration 0 5))
                  '((((15 25) (105 135)) (17))))))
