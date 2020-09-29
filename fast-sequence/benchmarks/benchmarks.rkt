#lang racket

(require fast-sequence
         (submod fast-sequence private-for-testing)
         "../private/fast-sequence-testing.rkt"
         "nest.rkt")

(define ITER-CT 500000)

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

(time* "merge"
 (for/list ([(x y) (in-merge (in-list '(1 2 3 4 5))
                             (in-list '(a b c d e)))])
   (list x y))
 (for/list ([(x) (in-list '(1 2 3 4 5))]
            [(y) (in-list '(a b c d e))])
   (list x y)))

(time* "nesting /w do-sequence2"
 (for/list ([x (in-nested ([(x) (in-list '((1 2 3) (4 5)))])
                              (do/sequence ([(y) (in-list x)]) y))])
   x)
 (for/list ([(x) (in-list '((1 2 3) (4 5)))]
            #:when #t
            [(z) (in-list x)])
   z))

;; 1. nesting using do/sequence
;; 2. hand-optimized multiple for-clauses
;; 3. naive dynamic sequence
;; 4. hand-optimized single for-clause

(time* "nesting /w dynamic sequence v.1"
       (for/list ([(x) (in-concat-sequences1 '((1 2 3) (4 5)))])
         x)
       (for/list ([(x) (in-list '((1 2 3) (4 5)))]
                  #:when #t
                  [(z) (in-list x)])
         z))

(time* "nesting /w dynamic sequence v.2"
       (for/list ([(x) (in-concat-sequences2 '((1 2 3) (4 5)))])
         x)
       (for/list ([(x) (in-list '((1 2 3) (4 5)))]
                  #:when #t
                  [(z) (in-list x)])
         z))

(time* "nesting /w dynamic sequence v.3"
       (for/list ([(x) (in-concat-sequences '((1 2 3) (4 5)))])
         x)
       (for/list ([(x) (in-list '((1 2 3) (4 5)))]
                  #:when #t
                  [(z) (in-list x)])
         z))

(time* "nesting /w dynamic sequence v.1.1"
       (for/list ([(x) (in-concat-sequences1
                        (sequence-map (lambda (x)
                                        (sequence-map (lambda (z) z) x))
                                      '((1 2 3) (4 5))))])
         x)
       (for/list ([(x) (in-list '((1 2 3) (4 5)))]
                  #:when #t
                  [(z) (in-list x)])
         z))

(time* "nesting /w dynamic sequence v.2.1"
       (for/list ([(x) (in-concat-sequences2
                        (sequence-map (lambda (x)
                                        (sequence-map (lambda (z) z) x))
                                      '((1 2 3) (4 5))))])
         x)
       (for/list ([(x) (in-list '((1 2 3) (4 5)))]
                  #:when #t
                  [(z) (in-list x)])
         z))

(time* "nesting /w dynamic sequence v.3.1"
       (for/list ([(x) (in-concat-sequences
                        (sequence-map (lambda (x)
                                        (sequence-map (lambda (z) z) x))
                                      '((1 2 3) (4 5))))])
         x)
       (for/list ([(x) (in-list '((1 2 3) (4 5)))]
                  #:when #t
                  [(z) (in-list x)])
         z))

(time* "nesting /w :do-in"
 (for/list ([x (:do-in ([(outer-seq) '((1 2 3) (4 5))])
                        (list? outer-seq)
                        ([outer-seq outer-seq]
                         [inner-seq '()])
                        #t
                        ([(x outer-seq inner-rest x-is-found)
                          (let loop* ([outer-seq outer-seq]
                                      [inner-seq inner-seq])
                            (cond [(pair? inner-seq)
                                   (let ([x (car inner-seq)]
                                         [inner-rest (cdr inner-seq)])
                                     (cond [(odd? x)
                                            (values x outer-seq inner-rest #t)]
                                           [else
                                            (loop* outer-seq inner-rest)]))]
                                  [(pair? outer-seq)
                                   (let ([inner-lst (car outer-seq)]
                                         [outer-rest (cdr outer-seq)])
                                     (list? inner-lst)
                                     (loop* outer-rest inner-lst))]
                                  [else
                                   (values #f #f #f #f)]))])
                        x-is-found
                        #t
                        (outer-seq inner-rest))])
    x)
 (for/list ([(x) (in-list '((1 2 3) (4 5)))]
            #:when #t
            [(z) (in-list x)])
   z))

(time* "nesting /w :do-in v.2"
 (for/list ([x (:do-in ([(outer-seq) '((1 2 3) (4 5))])
                       (list? outer-seq)
                       ([outer-seq outer-seq]
                        [inner-seq '()]
                        [inner-is-initialized? #f]
                        [post-guard* #t]
                        [i-post-guard* #t])
                       #t
                       ([(x outer-seq* inner-rest inner-is-initialized? post-guard* i-post-guard* x-is-found)
                         (let ()
                           (define (loop-with-inner outer-seq* inner-seq* post-guard* i-post-guard*)
                             (cond [(pair? inner-seq*)
                                    (let ([x (car inner-seq*)]
                                          [inner-rest (cdr inner-seq*)])
                                      (cond [(odd? x)
                                             (cond [(and #t i-post-guard*)
                                                    (values x outer-seq* inner-rest #t #t #t #t)]
                                                   [else
                                                    (loop-without-inner outer-seq* post-guard*)])]
                                            [else
                                             (loop-with-inner outer-seq* inner-rest post-guard* #t)]))]
                                   [else
                                    (loop-without-inner outer-seq* post-guard*)]))
                           (define (loop-without-inner outer-seq* post-guard*)
                             (cond [(pair? outer-seq*)
                                    (cond [(and #t post-guard*)
                                           (let ([inner-lst (car outer-seq*)]
                                                 [outer-rest (cdr outer-seq*)])
                                             (list? inner-lst)
                                             (loop-with-inner outer-rest inner-lst #t #t))]
                                          [else
                                           (outer-is-done)])]
                                   [else
                                    (outer-is-done)]))
                           (define (outer-is-done)
                             (values #f #f #f #f #f #f #f))
                           (cond [inner-is-initialized?
                                  (loop-with-inner outer-seq inner-seq post-guard* i-post-guard*)]
                                 [else
                                  (loop-without-inner outer-seq post-guard*)]))])
                       x-is-found
                       x-is-found
                       (outer-seq* inner-rest inner-is-initialized? post-guard* i-post-guard*))])
   x)
 (for/list ([(x) (in-list '((1 2 3) (4 5)))]
            #:when #t
            [(z) (in-list x)])
   z))

(time* "nesting /w do/sequence"
 (for/list ([x (do/sequence ([(x) (in-list '((1 2 3) (4 5)))]
                              #:when #t
                              [(z) (in-list x)])
                 z)])
   x)
 (for/list ([(x) (in-list '((1 2 3) (4 5)))]
            #:when #t
            [(z) (in-list x)])
   z))

(time* "deep nesting /w do/sequence"
 (for/list ([x (do/sequence ([(x) (in-list '(((1 2 3)) ((4 5) (1 2))))]
                             #:when #t
                             [(y) (in-list x)]
                             #:when #t
                             [(z) (in-list y)])
                 z)])
   x)
 (for/list ([(x) (in-list '(((1 2 3)) ((4 5) (1 2))))]
            #:when #t
            [(y) (in-list x)]
            #:when #t
            [(z) (in-list y)])
   z))

(time* "merge + nesting"
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
