#lang racket

(require (for-syntax racket/syntax
                     racket))

;; Type    ::= Built-in
;;           | Type* -> Type
;;           | (type-values Type ...) --- type of the (values e : Type ...) expression
;;           | Syntax[Grammar]
;;
;; Grammar ::= ...
;;           | Id
;;           | Expr[Env][Type]
;;
;; Env     ::= G | Env, Id : Type

(define-syntax (bind-inners stx)
  (syntax-case stx ()
    ;; stx :
    ;;   Syntax[(_ (([(outer-id : Id ...) outer-rhs : Expr[G][a]] ...)
    ;;              outer-check : Expr[G, outer-id : a, ...][b]
    ;;              ([loop-id : Id loop-expr : Expr[G, outer-id : a, ...][c]] ...)
    ;;              pos-guard : Expr[G, outer-id : a, ..., loop-id : c, ...][d]
    ;;              ([(inner-id : Id ...) inner-rhs : Expr[G, outer-id : a, ..., loop-id : c, ...][(type-values e ...)]] ...)
    ;;              pre-guard : Expr[G'][p]
    ;;              post-guard : Expr[G', body-bingings][q]
    ;;              (loop-arg : Expr[G', body-bingings][c] ...))
    ;;             (id : Id ...)
    ;;             f : Expr[G, id : e', ...][e' ... -> t]
    ;;             fail-expr : Expr[G, inner-id : e, ... ...])]
    ;; where body-bindings are from the context of the :do-in use,
    ;;       id ... are part of inner-id ...,
    ;;       G' = G, outer-id : a, ..., loop-id : c, ..., inner-id : e, ... ...
    [(_ (([(outer-id ...) outer-rhs] ...)
         outer-check
         ([loop-id loop-expr] ...)
         pos-guard
         ([(inner-id ...) inner-rhs] ...)
         pre-guard
         post-guard
         (loop-arg ...)) (id ...) f fail-expr)
     ;; ==>
     ;; inner-rhs : Expr[G, outer-id : a, ..., loop-id : c, ...][(e ...)] ...
     #'(let-values ([(inner-id ...) inner-rhs] ...)
         ;; G' = G, outer-id : a, ..., loop-id : c, ..., inner-id : e, ... ...
         ;;
         ;; inner-id  : e ... ...
         ;;
         ;; outer-id ... and loop-id ... should have been bound before:
         ;; outer-id  : a ...
         ;; loop-id   : c ...
         ;;
         ;; body-bingings : not in scope
         (if (or (not (and pre-guard      ; pre-guard  : Expr[G'][p]
                           post-guard))   ; post-guard : Expr[G', body-bingings][q]
                 ;; id : e' ...
                 ;; f : Expr[G, id : e', ...][e' ... -> t]
                 (f id ...))
             ;; inner-id  : e ... ...
             ;; body-bingings : not in scope
             ;; loop-arg  : Expr[G', body-bingings][c] ...
             (values inner-id ... ... loop-arg ...)
             fail-expr))]))   ; fail-expr : Expr[G, inner-id : e, ... ...]

(begin-for-syntax
  ;; fast-sequence-filter-transformer : Syntax[((id ...) f orig-seq-exrp)] Syntax[ExpandedForClause] -> Syntax[ExpandedForClause]
  ;; ExpandedForClause --- corresponds to the form of a syntax object returned by expand-for-clause
  (define (fast-sequence-filter-transformer stx expanded-for-clause)
    (syntax-case stx ()
      [((id ...) f orig-seq-exrp)
       (syntax-case expanded-for-clause ()
         ;; stx :
         ;;   Syntax[((id : Id ...)
         ;;           f : Expr[G, id : e', ...][e' ... -> t]
         ;;           (([(outer-id : Id ...) outer-rhs : Expr[G][a]] ...)
         ;;            outer-check : Expr[G, outer-id : a, ...][b]
         ;;            ([loop-id : Id loop-expr : Expr[G, outer-id : a, ...][c]] ...)
         ;;            pos-guard : Expr[G, outer-id : a, ..., loop-id : c, ...][d]
         ;;            ([(inner-id : Id ...) inner-rhs : Expr[G, outer-id : a, ..., loop-id : c, ...][(type-values e ...)]] ...)
         ;;            pre-guard : Expr[G'][p]
         ;;            post-guard : Expr[G', body-bingings][q]
         ;;            (loop-arg : Expr[G', body-bingings][c] ...)))]
         ;; where body-bindings are from the context of the :do-in use,
         ;;       G' = G, outer-id : a, ..., loop-id : c, ..., inner-id : e, ... ...
         [(([(outer-id ...) outer-rhs] ...)
           outer-check
           ([loop-id loop-expr] ...)
           pos-guard
           ([(inner-id ...) inner-rhs] ...)
           pre-guard
           post-guard
           (loop-arg ...))
          ;; ==>
          (with-syntax ([(loop-id* ...) (generate-temporaries #'(loop-id ...))]
                        [estx expanded-for-clause])
            (for-clause-syntax-protect
             #'[(id ...)
                (:do-in
                 ;; outer bindings
                 ([(outer-id ...) outer-rhs] ...)
                 ;; outer check
                 outer-check
                 ;; loop bindings
                 ([loop-id loop-expr] ...)
                 ;; pos check
                 pos-guard
                 ;; inner bindings
                 ([(inner-id ... ... loop-id* ...)
                   (bind-inners estx (id ...) f
                                ;; G* = G, outer-id : a, ..., loop-id : c, ...,
                                ;;      inner-id : e, ... ..., body-bingings
                                ;;
                                ;; outer-id ... and loop-id ... should have been bound before:
                                ;; outer-id  : a ...
                                ;; loop-id : c ...
                                ;;
                                ;; inner-id ... should be bound by bind-inners:
                                ;; inner-id : e ... ...
                                ;; body-bingings : not in scope
                                ;;
                                ;; loop-arg : Expr[G*][c] ...
                                (let loop ([loop-id loop-arg] ...)
                                  ;; loop-id[inner] : c ...
                                  ;; loop-id[inner] shadows loop-id
                                  ;;
                                  ;; pos-guard : Expr[G, outer-id : a, ..., loop-id : c, ...][d]
                                  (if pos-guard
                                      (bind-inners estx (id ...) f
                                                   (loop loop-arg ...))
                                      ;; G** = G, outer-id : a, ..., loop-id[inner] : c, ...,
                                      ;;       inner-id[inner] : e, ... ..., body-bingings
                                      ;;
                                      ;; inner-id[inner] ... should be bound by bind-inners:
                                      ;; inner-id[inner]  : e ... ...
                                      ;; inner-id[inner] ... shadow inner-id ...
                                      ;; loop-arg : Expr[G**][c] ...
                                      (values inner-id ... ... loop-arg ...))))])
                 ;; pre guard
                 (and pre-guard
                      ;; id ... are part of inner-id : e ..., which should have been bound before
                      ;; id : e' ...
                      ;; f : Expr[G, id : e', ...][e' ... -> t]
                      (f id ...))
                 ;; post guard
                 post-guard
                 ;; loop args
                 (loop-id* ...))]))]
         [else (raise-syntax-error #f "bad :do-in clause" expanded-for-clause #'orig-seq-exrp)])])))

(define-sequence-syntax fast-sequence-filter
  (lambda () #'sequence-filter)
  (lambda (stx)
    (syntax-case stx ()
      [[(id ...) (_ f seq-expr)]
       (with-syntax ([estx (expand-for-clause stx #'[(id ...) seq-expr])])
         (fast-sequence-filter-transformer #'((id ...) f seq-expr) #'estx))]
      [_ #f])))

;; ------------------------------------------------------------
;; Debug and helper functions

(define-syntax (exp-for-clause stx)
  (syntax-case stx ()
    [(_ ostx e) (expand-for-clause (syntax ostx) (syntax e))]))

(begin-for-syntax
  ;; ct-seen-table : Hash[SExpr => Boolean]
  ;; Indicates whether we've already expanded the expression with the
  ;; given key.
  (define ct-seen-table (make-hash))

  ;; ct-check-seen : SExpr -> Void
  ;; Checks that the given key hasn't been seen, and then add it to the
  ;; seen table.
  (define (ct-check-seen key)
    (when (hash-ref ct-seen-table key #f)
      (error 'ct-check-seen "already seen expression with key: ~e" key))
    (hash-set! ct-seen-table key #t))

  ;; make-keys : Syntax[(Expr ...) -> (Listof Int)]
  (define (make-keys exprs-stx)
    (build-list (length (syntax->list exprs-stx)) values))
  )

(define-syntax (once/compile-time stx)
  (syntax-case stx ()
    [(_ key expr)
     (begin
       (ct-check-seen (syntax->datum #'key))
       #'expr)]))

(begin-for-syntax
  ;; seq-check-once : Syntax[(key ForClause)] -> Syntax[ExpandedForClause]
  ;; ForClause --- a for clause of the form [(id ...) seq-expr]
  (define (seq-check-once stx)
    (syntax-case stx ()
      [(key [(id ...) seq-expr])
       (with-syntax ([estx (expand-for-clause stx #'[(id ...) seq-expr])])
         (syntax-case #'estx ()
           [(([(outer-id ...) outer-rhs] ...)
             outer-check
             ([loop-id loop-expr] ...)
             pos-guard
             ([(inner-id ...) inner-rhs] ...)
             pre-guard
             post-guard
             (loop-arg ...))
            ;; ==>
            (with-syntax ([(i1 ...) (make-keys #'(outer-rhs ...))]
                          [(i2 ...) (make-keys #'(loop-expr ...))]
                          [(i3 ...) (make-keys #'(inner-rhs ...))]
                          [(i4 ...) (make-keys #'(loop-arg ...))])
              #'(([(outer-id ...) (once/compile-time (key :outer-rhs i1) outer-rhs)] ...)
                 (once/compile-time (key :outer-check) outer-check)
                 ([loop-id (once/compile-time (key :loop-expr i2) loop-expr)] ...)
                 (once/compile-time (key :pos-guard) pos-guard)
                 ([(inner-id ...) (once/compile-time (key :inner-rhs i3) inner-rhs)] ...)
                 (once/compile-time (key :pre-guard) pre-guard)
                 (once/compile-time (key :post-guard) post-guard)
                 ((once/compile-time (key :loop-arg i4) loop-arg) ...)))]))])))

(define-sequence-syntax fast-sequence-filter-check-once
  (lambda () #'sequence-filter)
  (lambda (stx)
    (syntax-case stx ()
      [[(id ...) (_ key f seq-expr)]
       (fast-sequence-filter-transformer #'((id ...) f seq-expr) (seq-check-once #'(key [(id ...) seq-expr])))]
      [_ #f])))

(define-syntax (rewrite-to-slow stx)
  (syntax-case stx (fast-sequence-filter fast-sequence-map)
    [(_ (fast-sequence-filter f seq-expr))
     #'(sequence-filter f (rewrite-to-slow seq-expr))]
    [(_ (fast-sequence-map f seq-expr))
     #'(sequence-map f (rewrite-to-slow seq-expr))]
    [(_ seq-expr)
     #'seq-expr]))

(require rackunit
         (for-syntax syntax/parse
                     syntax/srcloc))

(define-syntax (check-fast-seq-combinators stx)
  (syntax-parse stx
    [(_ [(id ...) seq-expr]
        ...
        (~optional (~seq #:when guard-expr)))
     (with-syntax ([msg (string-append "Test failed at " (source-location->string stx))])
       #'(check-equal?
          (for/list ([(id ...) seq-expr]
                     ...
                     (~? (~@ #:when guard-expr) (~@)))
            (list  id ... ...))
          (for/list ([(id ...) (rewrite-to-slow seq-expr)]
                     ...
                     (~? (~@ #:when guard-expr) (~@)))
            (list id ... ...))
          msg))]))

;; ------------------------------------------------------------
;; Tests

(require "fast-sequence-map.rkt")

;(for/list ([(x) (fast-sequence-filter-check-once 1 (lambda (x) #t) (in-range 1 3))]) x)

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

(define (counter)
  (define n 0)
  (lambda ([d 1]) (set! n (+ d n)) n))

(check-fast-seq-combinators [(x) (fast-sequence-filter odd? (in-producer (counter)
                                                                         (lambda (x) (> x 10))))])

(check-fast-seq-combinators [(x) (fast-sequence-filter char-numeric?
                                                       (in-port read-char (open-input-string "a1b2c3")))])

(check-fast-seq-combinators [(x) (fast-sequence-filter
                                  even?
                                  (fast-sequence-map sqr (in-list (list 1 2 3 7 5 6))))])

(check-fast-seq-combinators [(x y) (fast-sequence-filter
                                    (lambda (x y) (odd? x))
                                    (fast-sequence-map (lambda (x) (values (sqr x) x))
                                                       (in-vector (vector 1 2 4 3 4))))])

(check-fast-seq-combinators [(x y) (fast-sequence-map (lambda (x) (values (add1 x) x))
                                                      (fast-sequence-filter
                                                       odd?
                                                       (fast-sequence-map sqr (in-vector (vector 1 2 4 3 4)))))])

#;(for ([(x) (fast-sequence-filter
              odd?
              (fast-sequence-map sqr (in-vector (vector 1 2 4 3 4))))])
    (println x))

#;(for ([(x y) (fast-sequence-map (lambda (x) (values (add1 x) x))
                                  (fast-sequence-filter
                                   odd?
                                   (fast-sequence-map sqr (in-vector (vector 1 2 4 3 4)))))])
    (println (list x y)))
