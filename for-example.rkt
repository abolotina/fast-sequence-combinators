#lang racket

(for/list ([(x) (in-list '(1 2 3))]
           #:when #t
           [(y) (in-range x)])
  y)