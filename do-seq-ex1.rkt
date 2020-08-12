#lang racket

(require "do-sequence-wo-protect.rkt")

(for/list ([x (do/sequence2* ([(x) (in-list '(1 2 3))])
                             (do/sequence ([(y) (in-range x)]) y))])
   x)
