#lang racket/base

;; Silence `racket-langserver` logger propagation to the root stderr logger
;; when required before production modules in a test module's `test` submodule.
;; Direct log receivers attached to `racket-langserver-logger` still observe events.
(unless (eq? (logger-name (current-logger)) 'racket-langserver-silent-test)
  (current-logger
    (make-logger 'racket-langserver-silent-test
                 (current-logger)
                 'none
                 'racket-langserver)))
