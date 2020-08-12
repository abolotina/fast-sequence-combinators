#lang racket

(require "do-sequence.rkt")

(for/list ([x (do/sequence2 ([(x) (in-list '(1 2 3))])
                            (in-range x))])
   x)
