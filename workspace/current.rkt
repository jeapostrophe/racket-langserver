#lang racket/base

(require "state.rkt")

(provide current-workspace)

(define current-workspace
  (make-workspace))
