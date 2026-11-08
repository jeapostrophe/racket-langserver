#lang racket/base

(require drracket/check-syntax
         rackunit
         racket/class
         "../../common/interfaces.rkt"
         "../../common/path-util.rkt"
         "../../common/source.rkt"
         "../../doclib/doc-trace.rkt"
         "../../doclib/internal-types.rkt"
         "../../doclib/lexer.rkt"
         "../../doclib/editor.rkt")

(module+ test
  (for ([names '((a) (struct:a a? a))])
    (test-case
      (format "definition targets ~s retain their shared range" names)
      (define path (build-path (current-directory) "m.rkt"))
      (define uri (path->uri path))
      (define text "#lang racket/base\n(provide a?)\n(struct a ())\n")
      (define doc-text (new lsp-editor%))
      (send doc-text insert text 0)
      (define trace
        (new build-trace%
          [source (path->source path)]
          [doc-text doc-text]
          [lexer-state (build-lexer-state text uri)]))
      ;; Check Syntax reports all three struct bindings at the written a.
      (for ([name (in-list names)])
        (send trace syncheck:add-definition-target/phase-level+space
              path 39 40 name '() 0))
      (check-equal?
        (for/hash ([entry (in-list
                            (send (send trace get-declaration) module-binding-definitions))])
          (values (cdr entry) (car entry)))
        (for/hash ([name (in-list names)])
          (values (Module-Binding path '() 0 name) (CharRange 39 40))))
      (check-equal?
        (Doc-Contribution-definitions (send trace get-contribution))
        (for/hash ([name (in-list names)])
          (values (Module-Binding path '() 0 name)
                  (Location uri (Range (Pos 2 8) (Pos 2 9)))))))))
