#lang racket

(require "do-sequence-wo-protect.rkt")

(for/list ([x (do/sequence* ([(x) (in-list '((1 2 3) (4 5)))]
                              #:when #t
                              [(z) (in-list x)])
                 z)])
   x)
