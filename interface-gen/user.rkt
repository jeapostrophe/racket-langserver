#lang racket
(require "interface.rkt")

(match #hasheq([line . 5] [character . 12])
  [(Position #:character c #:line l)
   (cons l c)])

(Position 2 3)

(match #hasheq([line . 5])
  [(Position #:line l)
   l])