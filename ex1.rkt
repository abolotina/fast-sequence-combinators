#lang racket

(;; loop bindings
 ([loop-id : Id loop-expr : Expr[G][L]])
 ;; pos check
 pos-guard : Expr[G/loop-id : L][Any]
 ;; inner bindings
 ([inner-id : Id inner-rhs : Expr[G/loop-id : L][I]] ...)
 ;; loop args
 (loop-arg : Expr[G/loop-id : L/inner-id : I, ...][L]))


(([lst1 #;A : Id '((1 2) () (2 4) (5 6)) : Expr[G][(Listof^2 Nat)]])
 (pair? lst1) : Expr[G/lst1 : (Listof Nat)][Boolean]
 ([x : Id (car lst1) : Expr[G/lst1 : (Listof^2 Nat)][(Listof Nat)]]
  [lst1 #;B : Id (cdr lst1) : Expr[G/lst1 : (Listof^2 Nat)][(Listof^2 Nat)])])
 (lst1 #;B : Expr[G/lst : (Listof Nat)/x : (Listof Nat), rest1 : (Listof^2 Nat)][(Listof^2 Nat)])),
where lst1, rest1 are fresh variables

(([lst2 : Id x : Expr[G/x : (Listof Nat)][(Listof Nat)]])
 (pair? lst2) : Expr[G/x : (Listof Nat)/lst2 : (Listof Nat)][Boolean]
 ([y : Id (car lst2) : Expr[G/x : (Listof Nat)/lst2 : (Listof Nat)][Nat]]
  [rest2 : Id (cdr lst2) : Expr[G/x : (Listof Nat)/lst2 : (Listof Nat)][(Listof Nat)])])
 (rest2 : Expr[G/x : (Listof Nat)/lst2 : (Listof Nat)/y : Nat, rest2 : (Listof Nat)][(Listof Nat)])),
where lst2, rest2 are fresh variables

==>

(([lst1 '((1 2) () (2 4) (5 6))]
  [x* #f]
  [lst1* #f]
  [lst2 #f]
  [inner-is-initialized? #f])
 #t
([(lst1* x* lst1* y rest2 y-is-found inner-is-initialized?)
  (let ()
    (define (loop-with-inner lst1* x* lst1** lst2*)
      (let ([lst1 #;A lst1*])
        (let ([x x*]
              [lst1 #;B lst1**])
          (let ([lst2 lst2*])
            (cond [(pair? lst2)
                   (let ([y (car lst2)]
                         [rest2 (cdr lst2)])
                     (values lst1 #;A x lst1 #;B y rest2 #t #t))] ;; Error is here!
                  [else (loop-without-inner lst1)])))))
    (define (loop-without-inner lst1)
      (cond [(pair? lst1)
             (let ([x (car lst1)]
                   [lst1 (cdr lst1)])
               (let ([lst1* lst1]
                     [lst2 x])
                 (loop-with-inner lst1* x lst1 lst2)))]
            [else (outer-is-done)]))
    (define (outer-is-done)
      (values #f #f #f #f #f #f #f))
    (cond [inner-is-initialized?
           (loop-with-inner lst1 x* lst1* lst2)]
          [else (outer-is-done)]))])
 (lst1*
  x*
  lst1*
  rest2))
