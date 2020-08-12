#lang racket

(require "do-sequence.rkt")

(for/list ([(x) (in-protect (in-list '(1 2 3 4 5)))]) x)
