#lang racket

(require (for-syntax racket/syntax))

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
          (with-syntax* ([estx expanded-for-clause]
                         [(loop-id* ...) (generate-temporaries #'(loop-id ...))]
                         [(loop-arg-id ...) (generate-temporaries #'(loop-arg ...))]
                         [(loop-arg* ...) #'(((loop-arg-id loop-id ...) inner-id ... ...) ...)]
                         [(false* ...) (build-list
                                       (length (syntax->list #'(inner-id ... ... loop-arg* ...)))
                                       (lambda (x) #'#f))])
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
                 ;; outer-id ... and loop-id ... should have been bound before:
                 ;; outer-id : a ...
                 ;; loop-id  : c ...
                 ;;
                 ;; pos-guard* : Expr[G, outer-id : a, ..., loop-id : c, ...][d]
                 #t
                 ;; inner bindings
                 ([(inner-id ... ... loop-id* ... ok)
                   (let ([loop-arg-id (lambda (loop-id ...) (lambda (inner-id ... ...) loop-arg))] ...)
                     (let loop ([loop-id loop-id] ...)
                       ;; outer-id ... and loop-id ... should have been bound before:
                       ;; outer-id : a ...
                       ;; loop-id  : c ...
                       ;;
                       ;; pos-guard* : Expr[G, outer-id : a, ..., loop-id : c, ...][d]
                       (if pos-guard
                           (let-values ([(inner-id ...) inner-rhs] ...)
                             ;; G' = G, outer-id : a, ..., loop-id : c, ..., inner-id : e, ... ...
                             ;;
                             ;; inner-id : e ... ...
                             (if (and pre-guard      ; pre-guard  : Expr[G'][p]
                                      post-guard)    ; post-guard : Expr[G'][q]
                                 ;; inner-id  : e ... ...
                                 ;; loop-arg* : Expr[G'][c] ...
                                 ;;
                                 ;; id ... are part of inner-id : e ..., which should have been bound before
                                 ;; id : e' ...
                                 ;; f  : Expr[G, id : e', ...][e' ... -> t]
                                 (if (f id ...)
                                     (values inner-id ... ... loop-arg* ... #t)
                                     (loop loop-arg* ...))
                                 (values false* ... #f)))
                           ;; false* : Expr[][Boolean] ...
                           (values false* ... #f))))])
                 ;; pre guard
                 ;; ok : Expr[][Boolean]
                 ok
                 ;; post guard
                 ;; ok : Expr[][Boolean]
                 ok
                 ;; G' = G, outer-id : a, ..., loop-id : c, ..., inner-id : e, ... ...
                 ;;
                 ;; outer-id ..., loop-id ..., and inner-id should have been bound before:
                 ;; outer-id  : a ...
                 ;; loop-id   : c ...
                 ;; inner-id  : e ... ...
                 ;;
                 ;; loop-arg* : Expr[G'][c] ...
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
