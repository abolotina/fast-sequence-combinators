#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     racket/list
                     racket/match)
         racket/bool
         racket/sequence
         racket/list
         racket/math
         racket/pretty
         fast-sequence
         (submod fast-sequence private-for-testing)
         "../../private/fast-sequence-testing.rkt"
         "../nest.rkt")

(define ITER-CT 1000000)

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
    (let-values
        ([(results t real-t gc-t)
          (time-apply
           (lambda ()
             (for ([i (in-range
                       (quotient ITER-CT divisor))])
               expr))
           null)])
      t)))

(define-syntax (time* stx)
  (define-syntax-rule (match-mode mode [pat body] ...)
    (syntax-parse mode
      #:literals (quote)
      [(quote datum)
       (match (syntax->datum #'datum)
         [pat body] ...)]))
  (syntax-parse stx
    [(_ label:str nest-level:nat
        (~optional (~seq #:mode mode) #:defaults ([mode #''list-of-lists]))
        (~optional (~seq #:out-file filename))
        expr1:expr (~optional expr2:expr))
     (let* ([nl (syntax-parse #'nest-level
                  [n:nat (syntax->datum #'n)])]
            [ids (generate-temporaries (make-list (add1 nl) 'l))])
       (with-syntax*
           ([(length-lst seq)
             (match nl
               [0 (list #'lengths (car ids))]
               [1 (list #'length-pairs
                        #`(make-list #,(car ids)
                            #,(match-mode (attribute mode)
                                ['list-of-lists #`(range #,(cadr ids))]
                                ['list-of-ranges (cadr ids)])))]
               [2 (list #'length-triples
                        #`(make-list #,(car ids)
                            (make-list #,(cadr ids)
                              #,(match-mode (attribute mode)
                                  ['list-of-lists #`(range #,(caddr ids))]
                                  ['list-of-ranges (caddr ids)]))))])])
         (define (benchmarks-apply expr-stx)
           #`(for/list ([ls (in-list length-lst)])
               (let*-values ([(ls-lst) (flatten ls)]
                             [#,ids (apply values ls-lst)]
                             [(#,(datum->syntax stx 'S)) seq]
                             [(d) (apply * (list #,@ids))])
                 (list ls-lst #,expr-stx))))
         #`(let ()
             (define-sequence-rule (#,(datum->syntax stx 'in-inner) seq-expr)
               #,(match-mode (attribute mode)
                   ['list-of-lists #'(in-list seq-expr)]
                   ['list-of-ranges #'(in-range seq-expr)]))
             (define out #,(if (attribute filename)
                               #'(open-output-file filename #:exists 'replace)
                               #'(current-output-port)))
             (pretty-write
              (list (list label #,(benchmarks-apply #'(time** expr1 d)))
                    #,@(if (attribute expr2)
                           #`((list "Solution w/o do/sequence"
                                    #,(benchmarks-apply #'(time** expr2 d))))
                           #'()))
              out)
             #,@(if (attribute filename)
                    #'((close-output-port out))
                    #'()))))]))

(define (length* x)
  (cond
    [(list? x) (length x)]
    [(natural? x) x]))

(define out-path
  "srcs/")

;; ---------------------------------------------
;; benchmarks

(time* "Nesting w/ do/sequence: general when-clauses (Opt. 1+3)" 1
       #:out-file (string-append out-path "opt1+3.txt")
       (for/list ([x (do/sequence ([(x) (in-list S)]
                                   #:when true
                                   [(z) (in-inner x)])
                       z)])
         x))

(time* "Nesting w/ do/sequence: general when-clauses (Opt. 1)" 1
       #:out-file (string-append out-path "opt1.txt")
       (for/list ([x (do/sequence-opt1 ([(x) (in-list S)]
                                        #:when true
                                        [(z) (in-inner x)])
                       z)])
         x))

(time* "Nesting w/ do/sequence: general when-clauses (Opt. 2)" 1
       #:out-file (string-append out-path "opt2.txt")
       (for/list ([x (do/sequence-opt2 ([(x) (in-list S)]
                                        #:when true
                                        [(z) (in-inner x)])
                       z)])
         x))

(time* "Nesting w/ do/sequence: general when-clauses (Opt. 3)" 1
       #:out-file (string-append out-path "opt3.txt")
       (for/list ([x (do/sequence-opt3 ([(x) (in-list S)]
                                        #:when true
                                        [(z) (in-inner x)])
                       z)])
         x))

(time* "Nesting w/ do/sequence: general when-clauses (W/o opt.)" 1
       #:out-file (string-append out-path "wo-opt.txt")
       (for/list ([x (do/sequence-w/o-opt ([(x) (in-list S)]
                                           #:when true
                                           [(z) (in-inner x)])
                       z)])
         x)
       (for/list ([(x) (in-list S)]
                  #:when true
                  [(z) (in-inner x)])
         z))

(when #f
  (time* "merge" 0
         (for/list ([(x y) (in-merge (in-range S)
                                     (in-range S))])
           (list x y))
         (for/list ([(x) (in-range S)]
                    [(y) (in-range S)])
           (list x y)))

  (time* "nesting w/ do-sequence2" 1
         (for/list ([x (in-nested ([(x) (in-list S)])
                         (do/sequence ([(y) (in-inner x)]) y))])
           x)
         (for/list ([(x) (in-list S)]
                    #:when #t
                    [(z) (in-inner x)])
           z))

  ;; 1. nesting using do/sequence
  ;; 2. hand-optimized multiple for-clauses
  ;; 3. naive dynamic sequence
  ;; 4. hand-optimized single for-clause

  (time* "nesting w/ dynamic sequence v.1" 1
         (for/list ([(x) (in-concat-sequences1 S)])
           x)
         (for/list ([(x) (in-list S)]
                    #:when #t
                    [(z) (in-inner x)])
           z))

  (time* "nesting w/ dynamic sequence v.2" 1
         (for/list ([(x) (in-concat-sequences2 S)])
           x)
         (for/list ([(x) (in-list S)]
                    #:when #t
                    [(z) (in-inner x)])
           z))

  (time* "nesting w/ dynamic sequence v.3" 1
         (for/list ([(x) (in-concat-sequences S)])
           x)
         (for/list ([(x) (in-list S)]
                    #:when #t
                    [(z) (in-inner x)])
           z))

  (time* "nesting w/ dynamic sequence v.1.1" 1
         (for/list ([(x) (in-concat-sequences1
                          (sequence-map
                           (lambda (x) (sequence-map (lambda (z) z) x)) S))])
           x)
         (for/list ([(x) (in-list S)]
                    #:when #t
                    [(z) (in-inner x)])
           z))

  (time* "nesting w/ dynamic sequence v.2.1" 1
         (for/list ([(x) (in-concat-sequences2
                          (sequence-map
                           (lambda (x) (sequence-map (lambda (z) z) x)) S))])
           x)
         (for/list ([(x) (in-list S)]
                    #:when #t
                    [(z) (in-inner x)])
           z))

  (time* "nesting w/ dynamic sequence v.3.1" 1
         (for/list ([(x) (in-concat-sequences
                          (sequence-map
                           (lambda (x) (sequence-map (lambda (z) z) x)) S))])
           x)
         (for/list ([(x) (in-list S)]
                    #:when #t
                    [(z) (in-inner x)])
           z))

  (time* "nesting w/ :do-in" 1 #:mode 'list-of-lists
         (for/list ([x (:do-in ([(outer-seq) S])
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
         (for/list ([(x) (in-list S)]
                    #:when #t
                    [(z) (in-list x)])
           z))

  (time* "nesting w/ :do-in v.2" 1 #:mode 'list-of-lists
         (for/list ([x (:do-in ([(outer-seq) S])
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
         (for/list ([(x) (in-list S)]
                    #:when #t
                    [(z) (in-list x)])
           z))

  (time* "nesting w/ do/sequence: general when-clauses" 1
         (for/list ([x (do/sequence ([(x) (in-list S)]
                                     #:when (> (length* x) 3)
                                     [(z) (in-inner x)])
                                    z)])
           x)
         (for/list ([(x) (in-list S)]
                    #:when (> (length* x) 3)
                    [(z) (in-inner x)])
           z))

  (time* "deep nesting w/ do/sequence" 2
         (for/list ([x (do/sequence ([(x) (in-list S)]
                                     #:when #t
                                     [(y) (in-list x)]
                                     #:when #t
                                     [(z) (in-inner y)])
                                    z)])
           x)
         (for/list ([(x) (in-list S)]
                    #:when #t
                    [(y) (in-list x)]
                    #:when #t
                    [(z) (in-inner y)])
           z))

  (time* "deep nesting w/ do/sequence: general when-clauses" 2
         (for/list ([x (do/sequence ([(x) (in-list S)]
                                     #:when (> (length x) 3)
                                     [(y) (in-list x)]
                                     #:when (> (length* y) 3)
                                     [(z) (in-inner y)])
                                    z)])
           x)
         (for/list ([(x) (in-list S)]
                    #:when (> (length x) 3)
                    [(y) (in-list x)]
                    #:when (> (length* y) 3)
                    [(z) (in-inner y)])
           z))

  (time* "merge + nesting" 1
         (for/list ([x (do/sequence ([(x) (in-list S)]
                                     [(y) (in-list '(a b c d e))]
                                     #:when #t
                                     [(z) (in-inner x)])
                                    (list z y))])
           x)
         (for/list ([(x) (in-list S)]
                    [(y) (in-list '(a b c d e))]
                    #:when #t
                    [(z) (in-inner x)])
           (list z y)))

  (time* "merge + nesting: general when-clauses" 1
         (for/list ([x (do/sequence ([(x) (in-list S)]
                                     [(y) (in-list '(a b c d e))]
                                     #:when (> (length* x) 3)
                                     [(z) (in-inner x)])
                                    (list z y))])
           x)
         (for/list ([(x) (in-list S)]
                    [(y) (in-list '(a b c d e))]
                    #:when (> (length* x) 3)
                    [(z) (in-inner x)])
           (list z y))))
