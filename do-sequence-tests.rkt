#lang racket

(require "fast-sequence-testing.rkt"
         "fast-sequence-testing-wo-protect.rkt")

(test-do/seq

 (for/list ([(x) (do/sequence () 1)])
   2)

 (for/list ([(x) (do/sequence (#:when #t) 1)])
   x)

 (for/list ([(x) (do/sequence (#:when #f) 1)])
   x)

 (for/list ([x (do/sequence ([(x) (in-list '(1 2 3 4 5))] #:when (odd? x)) x)])
   x)

 (for/list ([x (do/sequence (#:when #t
                             [(x) (in-list '(1 2 3 4 5))]
                             [(y) (in-list '(a b c d e))])
                 (list x y))])
   x)

 (for/list ([x (do/sequence (#:when #t
                             [x (in-list '(1 2 3 4 5))]
                             [y (in-list '(#\A #\B #\c #\d #\e))]
                             #:when (odd? x)
                             #:when (char-upper-case? y))
                 (list x y))])
   x)

 (for/list ([(x) (do/sequence (#:when #t #:when (odd? 2) [(x*) (in-list '(1 2 3 4 5))]) x*)])
   x)

 (define a 1)
 (for/list ([a (do/sequence ([(a) (in-list '(1 2 3 4 5))]
                             [(z) (in-list '(a b c d e))]
                             #:when (odd? a))
                 (list a z))])
   a)

 (for/list ([a (do/sequence ([(y) (in-list '(1 2 3 4 5))] #:when (odd? y)
                             [(z) (in-list '(a b c d e))])
                 (list y z))])
   a)

 (for/list ([(x) (do/sequence ([(x) (in-list '((1 2) (3 4) (5 6)))]
                               #:when #t
                               [(y) (in-list x)])
                   y)])
   x)

 (for/list ([(x) (do/sequence (#:when #t
                               [(x) (in-list '((1 2) (3 4) (5 6)))]
                               #:when #t
                               [(y) (in-list x)])
                   y)])
   x)

 (for/list ([(x) (do/sequence (#:when #f
                               #:when #t
                               [(x) (in-list '((1 2) (3 4) (5 6)))]
                               #:when #t
                               [(y) (in-list x)])
                   y)])
   x)

 (for/list ([(x) (do/sequence ([(lst) '((1 2) () (2 4) (5 6))]
                               #:when #t
                               [(x) (in-list lst)]
                               #:when (odd? x))
                   x)])
   x)

 (for/list ([(y) (do/sequence ([(x) (do/sequence ([(seq) (in-list '((1 2) (3 4) (5 6)))]
                                                  #:when #t
                                                  [(x*) (in-list seq)])
                                      x*)])
                   x)])
   y)
#|
 (for/list ([(y) (do/sequence ([(outer-lst1) (in-list '(((1 2) (3 7) () (2 4) (5 6))
                                                        ((1 2) (2 4))
                                                        ((1 2) (3 4))))]
                               [(outer-lst2) (in-list '(((1 2) (3 7) () (2 4) (5 6))
                                                        ((1 2) (2 4))
                                                        ((1 2) (3 4))))]
                               #:when (odd? (caadr outer-lst1))
                               [(inner-lst) (in-list outer-lst1)]
                               #:when (and (pair? inner-lst) (odd? (car inner-lst)))
                               [(x) (in-list inner-lst)]
                               #:when (odd? x))
                   (list x outer-lst2))])
   y)

 (for/list ([(y) (do/sequence ([(outer-lst1) (in-list '(((1 2) (3 7) () (2 4) (5 6))
                                                        ((1 2) (2 4))
                                                        ((1 2) (3 4))))]
                               [(outer-lst2) (in-list '(((1 2) (3 7) () (2 4) (5 6))
                                                        ((1 2) (2 4))
                                                        ((1 2) (3 4))))]
                               #:when (odd? (caadr outer-lst1))
                               [x (do/sequence ([(inner-lst) (in-list outer-lst1)]
                                                #:when (and (pair? inner-lst) (odd? (car inner-lst)))
                                                [(x) (in-list inner-lst)]
                                                #:when (odd? x))
                                    x)])
                   (list x outer-lst2))])
   y)

 (for/list ([(y) (do/sequence ([(outer-lst1) (in-list '(((1 2) (3 7) () (2 4) (5 6))
                                                        ((1 2) (2 4))
                                                        ((1 2) (3 4))))]
                               [(outer-lst2) (in-list '(((1 2) (3 7) () (2 4) (5 6))
                                                        ((1 2) (2 4))
                                                        ((1 2) (3 4))))]
                               #:when (odd? (caadr outer-lst1))
                               [(inner-lst) (in-list outer-lst1)]
                               #:when (and (pair? inner-lst) (odd? (car inner-lst)))
                               [(x) (in-list inner-lst)]
                               #:when (odd? x)
                               [z (in-value x)])
                   (list x z outer-lst2))])
   y)

 (for/list ([(y) (do/sequence ([(outer-lst1) (in-list '(((1 2) (3 7) () (2 4) (5 6))
                                                        ((1 2) (2 4))
                                                        ((1 2) (3 4))))]
                               [(outer-lst2) (in-list '(((1 2) (3 7) () (2 4) (5 6))
                                                        ((1 2) (2 4))
                                                        ((1 2) (3 4))))]
                               #:when (odd? (caadr outer-lst1))
                               [x (do/sequence ([(inner-lst) (in-list outer-lst1)]
                                                #:when (and (pair? inner-lst) (odd? (car inner-lst)))
                                                [x (do/sequence ([(x) (in-list inner-lst)]
                                                                 #:when (odd? x)
                                                                 [(z) (in-value x)])
                                                     (list x z))])
                                               x)]
                               #:when #t
                               [y (in-list x)])
                   (list y outer-lst2))])
   y)
|#
 (for/list ([(x) (do/sequence ([(z) (in-value 1)]) z)])
   x)

 (define x 1)
 (define y 2)
 #;(for/list ([z (do/sequence ([x (in-list (list (list x y)))]
                             [() (in-nullary-relation (odd? x))])
                 x)])
    z)

 (for/list ([z (do/sequence ([x (in-list '(2 3 4))]) x)]) (+ x z))

 #;(for/list ([x (do/sequence2 ([x (list '(1 2))]) x)]) x)
)
