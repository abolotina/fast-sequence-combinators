#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     racket/list
                     racket/match)
         racket/sequence
         racket/list
         fast-sequence
         (submod fast-sequence private-for-testing)
         "../private/fast-sequence-testing.rkt"
         "nest.rkt")

(define ITER-CT 500000)

(define lengths '(10 25 50 100 300 500 700 1000))

(define length-pairs
  (for*/list ([x (in-list lengths)]
              [y (in-list lengths)])
    (list x y)))

(define length-triples
  (for*/list ([x (in-list lengths)]
              [y (in-list lengths)]
              [z (in-list lengths)])
    (list x y z)))

(define-syntax-rule (time** expr divisor)
  (begin
    (collect-garbage)
    (collect-garbage)
    (time
     (for ([i (in-range (quotient ITER-CT divisor))])
       expr))))

(define-syntax time*
  (syntax-parser
    [(_ label:str id:id nest-level:nat
        (~optional (~seq #:mode mode) #:defaults ([mode #''list-of-ranges]))
        expr1:expr expr2:expr)
     (let* ([nl (syntax-parse #'nest-level
                  [n:nat (syntax->datum #'n)])]
            [ids (generate-temporaries (make-list (add1 nl) 'l))])
       (with-syntax* ([(length-lst seq)
                       (match nl
                         [0 (list #'lengths (car ids))]
                         [1 (list #'length-pairs
                                  (syntax-parse (attribute mode)
                                    #:literals (quote)
                                    [(quote datum)
                                     (match (syntax->datum #'datum)
                                       ['list-of-lists
                                        #`(make-list #,(car ids)
                                                     (range #,(cadr ids)))]
                                       ['list-of-ranges
                                        #`(make-list #,(car ids)
                                                     #,(cadr ids))])]))]
                         [2 (list #'length-triples
                                  #`(build-list
                                     #,(car ids)
                                     (lambda (x) (make-list #,(cadr ids)
                                                            #,(caddr ids)))))])])
         #`(for ([ls length-lst])
             (let*-values ([(ls-lst) (flatten ls)]
                           [#,ids (apply values ls-lst)]
                           [(id) seq]
                           [(d) (apply * (list #,@ids))])
               (printf "~a ~a\n" label ls-lst)
               (time** expr1 d)
               (displayln "vs")
               (time** expr2 d)
               (display "\n")))))]))

;; ---------------------------------------------
;; benchmarks

(time* "merge" R 0
 (for/list ([(x y) (in-merge (in-range R)
                             (in-range R))])
   (list x y))
 (for/list ([(x) (in-range R)]
            [(y) (in-range R)])
   (list x y)))

(time* "nesting /w do-sequence2" L 1
 (for/list ([x (in-nested ([(x) (in-list L)])
                          (do/sequence ([(y) (in-range x)]) y))])
   x)
 (for/list ([(x) (in-list L)]
            #:when #t
            [(z) (in-range x)])
   z))

;; 1. nesting using do/sequence
;; 2. hand-optimized multiple for-clauses
;; 3. naive dynamic sequence
;; 4. hand-optimized single for-clause

(time* "nesting /w dynamic sequence v.1" L 1
       (for/list ([(x) (in-concat-sequences1 L)])
         x)
       (for/list ([(x) (in-list L)]
                  #:when #t
                  [(z) (in-range x)])
         z))

(time* "nesting /w dynamic sequence v.2" L 1
       (for/list ([(x) (in-concat-sequences2 L)])
         x)
       (for/list ([(x) (in-list L)]
                  #:when #t
                  [(z) (in-range x)])
         z))

(time* "nesting /w dynamic sequence v.3" L 1
       (for/list ([(x) (in-concat-sequences L)])
         x)
       (for/list ([(x) (in-list L)]
                  #:when #t
                  [(z) (in-range x)])
         z))

(time* "nesting /w dynamic sequence v.1.1" L 1
       (for/list ([(x) (in-concat-sequences1
                        (sequence-map
                         (lambda (x) (sequence-map (lambda (z) z) x)) L))])
         x)
       (for/list ([(x) (in-list L)]
                  #:when #t
                  [(z) (in-range x)])
         z))

(time* "nesting /w dynamic sequence v.2.1" L 1
       (for/list ([(x) (in-concat-sequences2
                        (sequence-map
                         (lambda (x) (sequence-map (lambda (z) z) x)) L))])
         x)
       (for/list ([(x) (in-list L)]
                  #:when #t
                  [(z) (in-range x)])
         z))

(time* "nesting /w dynamic sequence v.3.1" L 1
       (for/list ([(x) (in-concat-sequences
                        (sequence-map
                         (lambda (x) (sequence-map (lambda (z) z) x)) L))])
         x)
       (for/list ([(x) (in-list L)]
                  #:when #t
                  [(z) (in-range x)])
         z))

(time* "nesting /w :do-in" L 1 #:mode 'list-of-lists
 (for/list ([x (:do-in ([(outer-seq) L])
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
 (for/list ([(x) (in-list L)]
            #:when #t
            [(z) (in-list x)])
   z))

(time* "nesting /w :do-in v.2" L 1 #:mode 'list-of-lists
 (for/list ([x (:do-in ([(outer-seq) L])
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
 (for/list ([(x) (in-list L)]
            #:when #t
            [(z) (in-list x)])
   z))

(time* "nesting /w do/sequence" L 1
 (for/list ([x (do/sequence ([(x) (in-list L)]
                             #:when #t
                             [(z) (in-range x)])
                 z)])
   x)
 (for/list ([(x) (in-list L)]
            #:when #t
            [(z) (in-range x)])
   z))

(time* "nesting /w do/sequence: list of lists" L 1 #:mode 'list-of-lists
 (for/list ([x (do/sequence ([(x) (in-list L)]
                             #:when #t
                             [(z) (in-list x)])
                 z)])
   x)
 (for/list ([(x) (in-list L)]
            #:when #t
            [(z) (in-list x)])
   z))

(time* "nesting /w do/sequence: general when-clauses" L 1
 (for/list ([x (do/sequence ([(x) (in-list L)]
                             #:when (> x 3)
                             [(z) (in-range x)])
                 z)])
   x)
 (for/list ([(x) (in-list L)]
            #:when (> x 3)
            [(z) (in-range x)])
   z))

(time* "deep nesting /w do/sequence" L 2
 (for/list ([x (do/sequence ([(x) (in-list L)]
                             #:when #t
                             [(y) (in-list x)]
                             #:when #t
                             [(z) (in-range y)])
                 z)])
   x)
 (for/list ([(x) (in-list L)]
            #:when #t
            [(y) (in-list x)]
            #:when #t
            [(z) (in-range y)])
   z))

(time* "deep nesting /w do/sequence: general when-clauses" L 2
 (for/list ([x (do/sequence ([(x) (in-list L)]
                             #:when (> (length x) 3)
                             [(y) (in-list x)]
                             #:when (> y 3)
                             [(z) (in-range y)])
                 z)])
   x)
 (for/list ([(x) (in-list L)]
            #:when (> (length x) 3)
            [(y) (in-list x)]
            #:when (> y 3)
            [(z) (in-range y)])
   z))

(time* "merge + nesting" L 1
 (for/list ([x (do/sequence ([(x) (in-list L)]
                             [(y) (in-list '(a b c d e))]
                             #:when #t
                             [(z) (in-range x)])
                   (list z y))])
   x)
 (for/list ([(x) (in-list L)]
            [(y) (in-list '(a b c d e))]
            #:when #t
            [(z) (in-range x)])
   (list z y)))

(time* "merge + nesting: general when-clauses" L 1
 (for/list ([x (do/sequence ([(x) (in-list L)]
                             [(y) (in-list '(a b c d e))]
                             #:when (> x 3)
                             [(z) (in-range x)])
                   (list z y))])
   x)
 (for/list ([(x) (in-list L)]
            [(y) (in-list '(a b c d e))]
            #:when (> x 3)
            [(z) (in-range x)])
   (list z y)))
