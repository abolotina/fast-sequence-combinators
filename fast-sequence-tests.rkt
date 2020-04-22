#lang racket

(require "fast-sequence-testing.rkt")

(define (counter)
  (define n 0)
  (lambda ([d 1]) (set! n (+ d n)) n))
(define xs (list 1 2 3))

(test-once
 (check-fast-seq-combinators [(x) (fast-sequence-filter odd? (list 1 2 3 3 4))]
                             [(i) (in-naturals)])

 (check-fast-seq-combinators [(x) (fast-sequence-filter odd? (list 1 2 3 3 4))]
                             [(i) (in-naturals)]
                             #:when (> x 2))

 (check-fast-seq-combinators [(x) (fast-sequence-filter even? (in-list (list 1 2 3 7 5 6)))]
                             [(i) (in-naturals)])

 (check-fast-seq-combinators [(x) (fast-sequence-filter even? (in-list (list 1 2 3 7 5 6)))]
                             [(i) (in-naturals)]
                             #:when (> x 2))

 (check-fast-seq-combinators [(k v) (fast-sequence-filter (lambda (k v) (odd? v))
                                                          (in-hash (hash 'a 1 'b 2 'c 3)))]
                             [(i) (in-naturals)])

 (check-fast-seq-combinators [(k v) (fast-sequence-filter (lambda (k v) (odd? v))
                                                          (in-hash (hash 'a 1 'b 2 'c 3 'd 4 'e 5)))]
                             [(i) (in-naturals)]
                             #:when (< v 3))

 (check-fast-seq-combinators [(k v) (fast-sequence-filter (lambda (k v) (odd? v))
                                                          (in-immutable-hash (hash 'a 1 'b 2 'c 3)))]
                             [(i) (in-naturals)])

 (check-fast-seq-combinators [(k v) (fast-sequence-filter (lambda (k v) (odd? v))
                                                          (in-mutable-hash (make-hash
                                                                            '((a . 1) (b . 2) (c . 3)))))]
                             [(i) (in-naturals)])

 (check-fast-seq-combinators [(k v) (fast-sequence-filter (lambda (k v) (odd? v))
                                                          (in-weak-hash (make-weak-hash
                                                                         '((a . 1) (b . 2) (c . 3)))))]
                             [(i) (in-naturals)])

 (check-fast-seq-combinators [(b) (fast-sequence-filter odd? (in-bytes #"byte string"))])

 (check-fast-seq-combinators [(ch) (fast-sequence-filter char-numeric? (in-string "a1b2c3"))])

 (check-fast-seq-combinators [(x) (fast-sequence-filter odd? (in-vector (vector 1 2 4 3 4)))])

 (check-fast-seq-combinators [(x) (fast-sequence-filter odd? (in-producer (counter)
                                                                          (lambda (x) (> x 10))))])

 (check-fast-seq-combinators [(x) (fast-sequence-filter char-numeric?
                                                        (in-port read-char (open-input-string "a1b2c3")))])

 #;(check-fast-seq-combinators [(x) (fast-sequence-map char->integer
                                                        (in-port read-char (open-input-string "a1b2c3")))])
 
 (check-fast-seq-combinators [(x) (fast-sequence-filter
                                   even?
                                   (fast-sequence-map sqr (in-list (list 1 2 3 7 5 6))))])

 (check-fast-seq-combinators [(x) (fast-sequence-filter
                                   even?
                                   (fast-sequence-filter
                                    even?
                                    (fast-sequence-map sqr (in-list (list 1 2 3 7 5 6)))))])

 (check-fast-seq-combinators [(x y) (fast-sequence-filter
                                     (lambda (x y) (odd? x))
                                     (fast-sequence-map (lambda (x) (values (sqr x) x))
                                                        (in-vector (vector 1 2 4 3 4))))])

 #;(check-fast-seq-combinators [(x y) (fast-sequence-map (lambda (x) (values (add1 x) x))
                                                       (fast-sequence-filter
                                                        odd?
                                                        (fast-sequence-map sqr (in-vector (vector 1 2 4 3 4)))))])

 (check-fast-seq-combinators [(x y) (fast-sequence-map values xs xs)])

 (check-fast-seq-combinators [(x y) (fast-sequence-map values xs (in-string "abcd"))])

 (check-fast-seq-combinators [(x y) (fast-sequence-map values (in-port read-char (open-input-string "a1b2c3")) xs)]))

#;(for ([(x) (fast-sequence-filter
              odd?
              (fast-sequence-map sqr (in-vector (vector 1 2 4 3 4))))])
    (println x))

#;(for ([(x y) (fast-sequence-map (lambda (x) (values (add1 x) x))
                                  (fast-sequence-filter
                                   odd?
                                   (fast-sequence-map sqr (in-vector (vector 1 2 4 3 4)))))])
    (println (list x y)))