#lang racket/base

(require (for-syntax racket/syntax
                     syntax/unsafe/for-transform
                     racket/base
                     syntax/parse)
         (submod "../private/ecr.rkt" private)
         racket/sequence)

(provide fast-sequence-filter)

(module+ private-for-testing
  (provide (for-syntax fast-sequence-filter-transformer)))

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
       (syntax-parse expanded-for-clause
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
         [eb:expanded-clause-record
          ;; ==>
          (with-syntax* ([(loop-id* ...) (generate-temporaries #'(eb.loop-id ...))]
                         [(loop-arg* ...) (generate-temporaries #'(eb.loop-arg ...))]
                         [(false* ...) (build-list
                                       (length (syntax->list #'(eb.inner-id ... ... loop-arg* ...)))
                                       (lambda (x) #'#f))])
            (for-clause-syntax-protect
             #'[(id ...)
                (:do-in
                 ;; outer bindings
                 ([(eb.outer-id ...) eb.outer-rhs] ...
                  [(f*) f])
                 ;; outer check
                 eb.outer-check
                 ;; loop bindings
                 ([eb.loop-id eb.loop-expr] ...)
                 ;; pos check
                 #t
                 ;; inner bindings
                 ([(eb.inner-id ... ... loop-id* ... ok)
                   (let loop ([eb.loop-id eb.loop-id] ...)
                     (if eb.pos-guard
                         (let-values ([(eb.inner-id ...) eb.inner-rhs] ...)
                           (let ([loop-arg* eb.loop-arg] ...)
                             (if (and eb.pre-guard
                                      eb.post-guard)
                                 (if (f* id ...)
                                     (values eb.inner-id ... ... loop-arg* ... #t)
                                     (loop loop-arg* ...))
                                 (values false* ... #f))))
                         (values false* ... #f)))])
                 ;; pre guard
                 ok
                 ;; post guard
                 ok
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
