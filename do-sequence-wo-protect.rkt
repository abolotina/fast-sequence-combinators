#lang racket

(require (for-syntax racket/syntax
                     racket
                     syntax/parse
                     syntax/stx)
         "do-sequence.rkt")

(provide do/sequence*
         do/sequence2*)

(begin-for-syntax
  (define (merge* stx)
    (syntax-parse stx
      [(ecr:expanded-clause-record ...)
       #'(;; outer bindings
          ([(ecr.outer-id ...) ecr.outer-rhs] ... ...)
          ;; outer check
          (and ecr.outer-check ...)
          ;; loop bindings
          ([ecr.loop-id ecr.loop-expr] ... ...)
          ;; pos check
          (and ecr.pos-guard ...)
          ;; inner bindings
          ([(ecr.inner-id ...) ecr.inner-rhs] ... ...)
          ;; pre guard
          (and ecr.pre-guard ...)
          ;; post guard
          (and ecr.post-guard ...)
          ;; loop args
          (ecr.loop-arg ... ...))])))

(define-sequence-syntax do/sequence2*
  (lambda (stx)
    (raise-syntax-error #f "only allowed in a fast sequence context" stx))
  (lambda (stx)
    (syntax-parse stx
      [[(id:id ...) (_ (b-clause:bind-clause ...+) seq-expr:expr)] #:cut
       #:with eb:expanded-clause-record (merge* #'(b-clause.expanded ...))
       #:with eb-i:expanded-clause-record (expand-for-clause stx #'[(id ...) seq-expr])
       #:with ecr:expanded-clause-record (nest #'(eb eb-i))
       #'[(id ...)
          (:do-in
           ([(ecr.outer-id ...) ecr.outer-rhs] ...)
           ecr.outer-check
           ([ecr.loop-id ecr.loop-expr] ...)
           ecr.pos-guard
           ([(ecr.inner-id ...) ecr.inner-rhs] ...)
           ecr.pre-guard
           ecr.post-guard
           (ecr.loop-arg ...))]]
      [_ (raise-syntax-error #f "got something else" stx)])))

(define-sequence-syntax do/sequence*
  (lambda (stx)
    (raise-syntax-error #f "only allowed in a fast sequence context" stx))
  (lambda (stx)
    (syntax-parse stx
      [[(id:id ...) (_ () body:expr ...+)]
       (for-clause-syntax-protect
        #'[(id ...) (:do-in ([(id ...) body ...]) #t () #t () #t #f ())])]
      [[(id:id ...) (_ (w:when-chunk . rest) body:expr ...+)]
       (for-clause-syntax-protect
        #'[(id ...) (do/sequence2* ([() (in-nullary-relation w.expr)]) (do/sequence* rest body ...))])]
      [[(id:id ...) (_ (b:bind-chunk . rest) body:expr ...+)]
       (for-clause-syntax-protect
        #'[(id ...) (do/sequence2* (b.b ...) (do/sequence* rest body ...))])]
      [_ #f])))
