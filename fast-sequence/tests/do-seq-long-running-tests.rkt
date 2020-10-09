#lang racket/base

(require "../private/fast-sequence-testing.rkt"
         racket/match
         rackunit
         racket/list)

(define (counter)
  (define n 0)
  (lambda ([d 1]) (set! n (+ d n)) n))

(define (dyn-list l)
  (make-do-sequence
   (lambda ()
     (values car #f cdr l pair? #f #f))))

(define (dyn-int v)
  (make-do-sequence
   (lambda ()
     (values values add1 values 0 (lambda (i) (i . < . v)) #f #f))))

(define srcs `(([(in-list (list 1 2 3))
                 (list 1 2 3)
                 (in-range 5)
                 (in-value 1)
                 (dyn-int 5)
                 (dyn-list (list 1 2 3))
                 5
                 (+ 0 5)
                 (vector 1 2 3)
                 (in-vector (vector 3 5 7))
                 (fast-sequence-map add1 (list 2 3 4))
                 (in-producer (counter) (lambda (x) (> x 10)))]
                [,(lambda (x) #t)
                 ,(lambda (x) `(even? ,x))
                 ,(lambda (x) `(< ,x 5))
                 ,(lambda (x) `(> ,x 1))])
               
               ([(in-list (list #\a #\b #\c))
                 (in-string "hello")
                 (fast-sequence-filter char? (in-list (list #\4 1 #\f 5)))
                 (in-port read-char (open-input-string "a1b2c3"))]
                [,(lambda (x) #t)
                 ,(lambda (x) `(char-alphabetic? ,x))
                 ,(lambda (x) `(char<? ,x #\b))])))

(define seq-when-pairs*
  (for*/list ([l (in-list srcs)]
              [s (in-list (car l))]
              [w (in-list (cadr l))])
    (list s w)))

(define (merges seq1 seq2)
  (for*/list ([sw1 (in-list seq1)]
              [sw2 (in-list seq2)])
    `((in-merge ,(car sw1) ,(car sw2))
      ,(lambda (x y) `(and ,((cadr sw1) x) ,((cadr sw2) y))))))

(define seq-when-pairs  
  (append seq-when-pairs*
          (merges seq-when-pairs* seq-when-pairs*)))

(define (make-ids sw ids)
  (match (car sw)
    [(list* 'in-merge rest) ids]
    [e (list (car ids))]))

(define (bind-when sw ids)
  (let ([ids* (make-ids sw ids)])
    (values `[,ids* ,(car sw)] (apply (cadr sw) ids*))))

(define (make-test sw1 . sws)
  (define o (open-output-string))
  (cond
    [(empty? sws)
     (let*-values ([(b1 w1) (bind-when sw1 '(x y))]
                   [(ids) (car b1)]
                   [(do/seq) `(for/list ([x (do/sequence (,b1 #:when ,w1) (list ,@ids))]) x)])
       `(begin
          (display ,(begin (fprintf o "sw: [~a, ~a]\n" b1 w1)
                           (get-output-string o)))
          (check-equal?
           ,do/seq
           (for/list ([x (for/list (,b1 #:when ,w1) (list ,@ids))]) x)
           ,(begin
              (display do/seq o)
              (get-output-string o)))))]
    [else
     (let*-values ([(b1 w1) (bind-when sw1 '(x y))]
                   [(b2 w2) (bind-when (car sws) '(z w))]
                   [(ids) (car b2)]
                   [(do/seq)`(for/list ([x (do/sequence (,b1 #:when ,w1 ,b2 #:when ,w2)
                                             (list ,@ids))])
                               x)])
       `(begin
          (display ,(begin (fprintf o "sw1: [~a, ~a]\n sw2: [~a, ~a]\n" b1 w1 b2 w2)
                           (get-output-string o)))
          (check-equal?
           ,do/seq
           (for/list ([x (for/list (,b1 #:when ,w1 ,b2 #:when ,w2) (list ,@ids))]) x)
           ,(begin
              (display do/seq o)
              (get-output-string o)))))]))

(define (make-do/seq2-test sw1 sw2)
  (define o (open-output-string))
  (let*-values ([(b1 w1) (bind-when sw1 '(x y))]
                [(b2) `[z ,(car sw2)]]
                [(s2) (car sw2)]
                [(do/seq) `(for/list ([x (in-nested (,b1) ,s2)])
                             x)])
    `(begin
       (display ,(begin (fprintf o "b1: ~a\n s2: ~a\n" b1 s2)
                        (get-output-string o)))
       (check-equal?
        ,do/seq
        (for/list ([x (for/list (,b1 #:when #t ,b2) z)]) x)
        ,(begin
           (display do/seq o)
           (get-output-string o))))))

(define seq-when-pairs2
  (for*/list ([s (in-list (caar srcs))]
              [w (in-list (cadar srcs))])
    (list s w)))

(define seq-when-pairs3*
  (for*/list ([s (in-list '((in-range x) (in-value x) x))]
              [w (in-list (cadar srcs))])
    (list s w)))

(define seq-when-pairs3  
  (append seq-when-pairs3*
          (merges seq-when-pairs3* seq-when-pairs*)))

(define (run-tests1 ns)
  (for* ([sw (in-list seq-when-pairs)])
    (eval (make-test sw) ns)))

(define (run-tests2 ns)
  (for* ([sw1 (in-list seq-when-pairs)]
         [sw2 (in-list seq-when-pairs)])
    (eval (make-test sw1 sw2) ns)))

(define (run-tests3 ns)
  (for* ([sw1 (in-list seq-when-pairs2)]
         [sw2 (in-list seq-when-pairs3)])
    (eval (make-test sw1 sw2) ns)))

(define (run-tests4 ns)
  (for* ([sw1 (in-list seq-when-pairs2)]
         [sw2 (in-list seq-when-pairs3*)])
    (eval (make-do/seq2-test sw1 sw2) ns)))

(define (run-tests ns)
  (parameterize
      ([error-escape-handler
        (lambda ()
          (abort-current-continuation
           (default-continuation-prompt-tag)
           (exit)))])
    (begin
      (run-tests1 ns)
      (run-tests2 ns)
      (run-tests3 ns)
      (run-tests4 ns))))

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))
#;(eval (make-test (cadr seq-when-pairs)) ns)

(run-tests ns)
