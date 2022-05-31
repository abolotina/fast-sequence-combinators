#lang racket/base

(require (for-syntax racket/syntax
                     syntax/unsafe/for-transform
                     racket/base
                     syntax/parse)
         (submod "../private/ecr.rkt" private))

(provide fast-sequence-map)

(define-sequence-syntax fast-sequence-map
  (lambda () #'sequence-map)
  (lambda (stx)
    (syntax-case stx ()
      [[(id ...) (_ f seq-expr ...)]
       (with-syntax* ([(temp-id ...) (generate-temporaries #'(seq-expr ...))]
                      [estx (map (lambda (temp seq)
                                   (expand-for-clause stx #`[(#,temp) #,seq]))
                                 (syntax->list #'(temp-id ...))
                                 (syntax->list #'(seq-expr ...)))])
         (syntax-parse #'estx
           [(eb:expanded-clause-record ...)
            ;; ==>
            (with-syntax ([(ok) (generate-temporaries #'(ok))]
                          [(false* ...) (build-list
                                         (length (syntax->list #'(eb.inner-id ... ... ... id ...)))
                                         (lambda (x) #'#f))])
              (for-clause-syntax-protect
               #'[(id ...)
                  (:do-in
                   ;;outer bindings
                   ([(eb.outer-id ...) eb.outer-rhs] ... ...
                    [(f*) f])
                   ;; outer check
                   (and eb.outer-check ...)
                   ;; loop bindings
                   ([eb.loop-id eb.loop-expr] ... ...)
                   ;; pos check
                   (and eb.pos-guard ...)
                   ;; inner bindings
                   ([(eb.inner-id ... ... ... id ... ok)
                     (let-values ([(eb.inner-id ...) eb.inner-rhs] ... ...)
                       (cond
                         [(and eb.pre-guard ...)
                          (let-values ([(id ...) (f* temp-id ...)])
                            (values eb.inner-id ... ... ... id ... #t))]
                         [else
                          (values false* ... #f)]))])
                   ;; pre guard
                   ok
                   ;; post guard
                   (and eb.post-guard ...)
                   ;; loop args
                   (eb.loop-arg ... ...))]))]
           [else (raise-syntax-error #f "bad syntax" #'estx)]
           ))]
      [_ #f])))
