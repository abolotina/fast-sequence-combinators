#lang racket

(;; loop bindings
 ([loop-id : Id loop-expr : Expr[G][L]])
 ;; pos check
 pos-guard : Expr[G/loop-id : L][Any]
 ;; inner bindings
 ([inner-id : Id inner-rhs : Expr[G/loop-id : L][I]] ...)
 ;; loop args
 (loop-arg : Expr[G/loop-id : L/inner-id : I, ...][L]))

(for/list ([x #;B (do/sequence2 ([x #;A (in-list '((1 2) () (2 4) (5 6)))])
                                (in-list x))])
  x)

let mark = (make-mark-as-variables #'x)
let   x^ = (mark #'x)

;; outer
[x^ (in-list '((1 2) () (2 4) (5 6)))]
==>
((x^)
(([lst1 : Id '((1 2) () (2 4) (5 6)) : Expr[G][(Listof^2 Nat)]])
 (pair? lst1) : Expr[G/lst1 : (Listof Nat)][Boolean]
 ([x^ : Id (car lst1) : Expr[G/lst1 : (Listof^2 Nat)][(Listof Nat)]]
  [rest1 : Id (cdr lst1) : Expr[G/lst1 : (Listof^2 Nat)][(Listof^2 Nat)]])
 (rest1 : Expr[G/lst : (Listof Nat)/x^ : (Listof Nat), rest1 : (Listof^2 Nat)][(Listof^2 Nat)]))),
where lst1, rest1 are fresh variables

;; inner
[x (in-list x^)]
==>
((x)
(([lst2 : Id x^: Expr[G/x^: (Listof Nat)][(Listof Nat)]])
 (pair? lst2) : Expr[G/x^: (Listof Nat)/lst2 : (Listof Nat)][Boolean]
 ([x : Id (car lst2) : Expr[G/x^: (Listof Nat)/lst2 : (Listof Nat)][Nat]]
  [rest2 : Id (cdr lst2) : Expr[G/x^: (Listof Nat)/lst2 : (Listof Nat)][(Listof Nat)]])
 (rest2 : Expr[G/x^ : (Listof Nat)/lst2 : (Listof Nat)/x : Nat, rest2 : (Listof Nat)][(Listof Nat)]))),
where lst2, rest2 are fresh variables

==>

(([lst1 '((1 2) () (2 4) (5 6))]
  [x* #f]
  [rest1* #f]
  [lst2 #f]
  [inner-is-initialized? #f])
 #t
([(lst1* x* lst1* x rest2 y-is-found inner-is-initialized?)
  (let ()
    (define (loop-with-inner lst1* x* rest1* lst2*)
      (let ([lst1 lst1*])
        (let ([x^ x*] ; ∃(intdef-scope) (x-sym, x-A-scope U {intdef-scope})
              [rest1 rest1*])
          (let ([lst2 lst2*])
            (cond [(pair? lst2)
                   (let ([x (car lst2)] ; (x-sym, x-A-scope, x^, lst2)
                         [rest2 (cdr lst2)])
                     (values lst1 x^ rest1 x rest2 #t #t))] ; x^ is in the wrong scope!
                  ;; x^ should be in the scope of lst1 and x^
                  ;;; x^ is in the scope of lst1,  
                  [else (loop-without-inner lst1)])))))
    (define (loop-without-inner lst1)
      (cond [(pair? lst1)
             (let ([x (car lst1)]
                   [rest1 (cdr lst1)])
               (let ([lst1* lst1]
                     [lst2 x])
                 (loop-with-inner lst1* x rest1 lst2)))]
            [else (outer-is-done)]))
    (define (outer-is-done)
      (values #f #f #f #f #f #f #f))
    (cond [inner-is-initialized?
           (loop-with-inner lst1 x* rest* lst2)]
          [else (outer-is-done)]))])
 (lst1*
  x*
  rest1*
  rest2))
