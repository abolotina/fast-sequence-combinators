#lang racket

(provide (for-syntax fast-sequence-filter-transformer)
         fast-sequence-filter)

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
         ;;            post-guard : Expr[G'][q]
         ;;            (loop-arg : Expr[G'][c] ...)))]
         ;; where G' = G, outer-id : a, ..., loop-id : c, ..., inner-id : e, ... ...
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
                        [estx expanded-for-clause]
                        [(pos-guard-id inner-ids ... pre-guard-id post-guard-id)
                         (generate-temporaries #'(pos-guard inner-rhs ... pre-guard pos-guard))]
                        [(loop-arg-id ...) (generate-temporaries #'(loop-arg ...))])
            (for-clause-syntax-protect
             #'[(id ...)
                (:do-in
                 ;; outer bindings
                 ([(outer-id ... ... pos-guard-id inner-ids ...)
                   (let*-values ([(outer-id ...) outer-rhs] ...
                                 [(pos-guard-id) (lambda (loop-id ...) pos-guard)]
                                 [(inner-ids) (lambda (loop-id ...) inner-rhs)] ...)
                     (values outer-id ... ... pos-guard-id inner-ids ...))])
                 ;; outer check
                 outer-check
                 ;; loop bindings
                 ([loop-id loop-expr] ...)
                 ;; pos check
                 (pos-guard-id loop-id ...)
                 ;; inner bindings
                 ([(inner-id ... ... loop-id* ... pre-guard-id post-guard-id)
                   (let ([pre-guard-id (lambda (loop-id ...) (lambda (inner-id ... ...) pre-guard))]
                         [post-guard-id (lambda (loop-id ...) (lambda (inner-id ... ...) post-guard))]
                         [loop-arg-id (lambda (loop-id ...) (lambda (inner-id ... ...) loop-arg))] ...)
                     (bind-inners estx (id ...) (inner-ids ...) (loop-arg-id ...)
                                  pre-guard-id post-guard-id f
                                  ;; G' = G, outer-id : a, ..., loop-id : c, ...,
                                  ;;      inner-id : e, ... ...
                                  ;;
                                  ;; outer-id ... and loop-id ... should have been bound before:
                                  ;; outer-id  : a ...
                                  ;; loop-id : c ...
                                  ;;
                                  ;; inner-id ... should be bound by bind-inners:
                                  ;; inner-id : e ... ...
                                  ;;
                                  ;; loop-arg : Expr[G'][c] ...
                                  (let loop ([loop-id ((loop-arg-id loop-id ...) inner-id ... ...)] ...)
                                    ;; loop-id[inner] : c ...
                                    ;; loop-id[inner] shadows loop-id
                                    ;;
                                    ;; pos-guard : Expr[G, outer-id : a, ..., loop-id : c, ...][d]
                                    (if (pos-guard-id loop-id ...)
                                        (bind-inners estx (id ...) (inner-ids ...) (loop-arg-id ...)
                                                     pre-guard-id post-guard-id f
                                                     (loop ((loop-arg-id loop-id ...) inner-id ... ...) ...))
                                        ;; G'' = G, outer-id : a, ..., loop-id[inner] : c, ...,
                                        ;;       inner-id[inner] : e, ... ...
                                        ;;
                                        ;; inner-id[inner] ... should be bound by bind-inners:
                                        ;; inner-id[inner]  : e ... ...
                                        ;; inner-id[inner] ... shadow inner-id ...
                                        ;; loop-arg : Expr[G''][c] ...
                                        (values inner-id ... ... ((loop-arg-id loop-id ...) inner-id ... ...) ...
                                                pre-guard-id post-guard-id)))))])
                 ;; pre guard
                 (and ((pre-guard-id loop-id ...) inner-id ... ...)
                      ;; id ... are part of inner-id : e ..., which should have been bound before
                      ;; id : e' ...
                      ;; f : Expr[G, id : e', ...][e' ... -> t]
                      (f id ...))
                 ;; post guard
                 ((post-guard-id loop-id ...) inner-id ... ...)
                 ;; loop args
                 (loop-id* ...))]))]
         [else (raise-syntax-error #f "bad :do-in clause" expanded-for-clause #'orig-seq-exrp)])])))

(define-syntax (bind-inners stx)
  (syntax-case stx ()
    ;; stx :
    ;;   Syntax[(_ (([(outer-id : Id ...) outer-rhs : Expr[G][a]] ...)
    ;;              outer-check : Expr[G, outer-id : a, ...][b]
    ;;              ([loop-id : Id loop-expr : Expr[G, outer-id : a, ...][c]] ...)
    ;;              pos-guard : Expr[G, outer-id : a, ..., loop-id : c, ...][d]
    ;;              ([(inner-id : Id ...) inner-rhs : Expr[G, outer-id : a, ..., loop-id : c, ...][(type-values e ...)]] ...)
    ;;              pre-guard : Expr[G'][p]
    ;;              post-guard : Expr[G'][q]
    ;;              (loop-arg : Expr[G'][c] ...))
    ;;             (id : Id ...)
    ;;             f : Expr[G, id : e', ...][e' ... -> t]
    ;;             fail-expr : Expr[G, inner-id : e, ... ...])]
    ;; where id ... are part of inner-id ...,
    ;;       G' = G, outer-id : a, ..., loop-id : c, ..., inner-id : e, ... ...
    [(_ (([(outer-id ...) outer-rhs] ...)
         outer-check
         ([loop-id loop-expr] ...)
         pos-guard
         ([(inner-id ...) inner-rhs] ...)
         pre-guard
         post-guard
         (loop-arg ...))
        (id ...) (inner-ids ...) (loop-arg-id ...)
        pre-guard-id post-guard-id
        f fail-expr)
     ;; ==>
     ;; inner-rhs : Expr[G, outer-id : a, ..., loop-id : c, ...][(e ...)] ...
     #'(let*-values ([(inner-id ...) (inner-ids loop-id ...)] ...)
         ;; G' = G, outer-id : a, ..., loop-id : c, ..., inner-id : e, ... ...
         ;;
         ;; inner-id  : e ... ...
         ;;
         ;; outer-id ... and loop-id ... should have been bound before:
         ;; outer-id  : a ...
         ;; loop-id   : c ...
         (if (or (not (and ((pre-guard-id loop-id ...) inner-id ... ...)      ; pre-guard  : Expr[G'][p]
                           ((post-guard-id loop-id ...) inner-id ... ...)))   ; post-guard : Expr[G'][q]
                 ;; id : e' ...
                 ;; f : Expr[G, id : e', ...][e' ... -> t]
                 (f id ...))
             ;; inner-id  : e ... ...
             ;; loop-arg  : Expr[G', body-bingings][c] ...
             (values inner-id ... ... ((loop-arg-id loop-id ...) inner-id ... ...) ...
                     pre-guard-id post-guard-id)
             fail-expr))]))   ; fail-expr : Expr[G, inner-id : e, ... ...]

(define-sequence-syntax fast-sequence-filter
  (lambda () #'sequence-filter)
  (lambda (stx)
    (syntax-case stx ()
      [[(id ...) (_ f seq-expr)]
       (with-syntax ([estx (expand-for-clause stx #'[(id ...) seq-expr])])
         (fast-sequence-filter-transformer #'((id ...) f seq-expr) #'estx))]
      [_ #f])))
