#lang racket

(require "do-sequence.rkt"
         "do-sequence-wo-protect.rkt")

(define ITER-CT 50000)

(define-syntax-rule (time** expr)
  (begin
    (collect-garbage)
    (collect-garbage)
    (time
     (for ([i (in-range ITER-CT)])
       expr))))

(define-syntax-rule (time* label expr1 expr2)
  (begin
    (displayln label)
    (time** expr1)
    (displayln "vs")
    (time** expr2)
    (display "\n")))

;; ---------------------------------------------
;; benchmarks

(time* "protect"
 #;(for/list ([x (do/sequence ([(x) (in-list '(1 2 3 4 5))])
                 x)])
   x)
 (for/list ([(x) (in-protect (in-list '(1 2 3 4 5)))]) x)
 (for/list ([(x) (in-list '(1 2 3 4 5))]) x))

(time* "merge"
 #;(for/list ([x (do/sequence* ([(x) (in-list '(1 2 3 4 5))]
                              [(y) (in-list '(a b c d e))])
                 (list x y))])
   x)
 (for/list ([(x y) (in-merge (in-list '(1 2 3 4 5))
                             (in-list '(a b c d e)))])
   (list x y))
 (for/list ([(x) (in-list '(1 2 3 4 5))]
            [(y) (in-list '(a b c d e))])
   (list x y)))

(time* "nesting"
 (for/list ([x (do/sequence* ([(x) (in-list '((1 2 3) (4 5)))]
                              #:when #t
                              [(z) (in-list x)])
                 z)])
   x)
 (for/list ([(x) (in-list '((1 2 3) (4 5)))]
            #:when #t
            [(z) (in-list x)])
   z))

(time* "protect + merge"
 #;(for/list ([x (do/sequence ([(x) (in-list '(1 2 3 4 5))]
                             [(y) (in-list '(a b c d e))])
                   (list x y))])
   x)
 (for/list ([(x y) (in-merge (in-protect (in-list '(1 2 3 4 5)))
                             (in-protect (in-list '(a b c d e))))])
   (list x y))
 (for/list ([(x) (in-list '(1 2 3 4 5))]
            [(y) (in-list '(a b c d e))])
   (list x y)))

(time* "protect + nesting"
 (for/list ([x (do/sequence ([(x) (in-list '((1 2 3) (4 5)))]
                             #:when #t
                             [(z) (in-list x)])
                   z)])
   x)
 (for/list ([(x) (in-list '((1 2 3) (4 5)))]
            #:when #t
            [(z) (in-list x)])
   z))

(time* "merge + nesting"
 (for/list ([x (do/sequence* ([(x) (in-list '((1 2 3) (4 5)))]
                              [(y) (in-list '(a b c d e))]
                              #:when #t
                              [(z) (in-list x)])
                 (list z y))])
   x)
 (for/list ([(x) (in-list '((1 2 3) (4 5)))]
            [(y) (in-list '(a b c d e))]
            #:when #t
            [(z) (in-list x)])
   (list z y)))

(time* "protect + merge + nesting"
 (for/list ([x (do/sequence ([(x) (in-list '((1 2 3) (4 5)))]
                             [(y) (in-list '(a b c d e))]
                             #:when #t
                             [(z) (in-list x)])
                   (list z y))])
   x)
 (for/list ([(x) (in-list '((1 2 3) (4 5)))]
            [(y) (in-list '(a b c d e))]
            #:when #t
            [(z) (in-list x)])
   (list z y)))
