#lang racket

(require (for-syntax racket/syntax
                     syntax/unsafe/for-transform))

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
         (syntax-case #'estx ()
           [((([(outer-id ...) outer-rhs] ...)
              outer-check
              ([loop-id loop-expr] ...)
              pos-guard
              ([(inner-id ...) inner-rhs] ...)
              pre-guard
              post-guard
              (loop-arg ...)) ...)
            ;; ==>
            (with-syntax ([(false* ...) (build-list
                                         (length (syntax->list #'(inner-id ... ... ... id ...)))
                                         (lambda (x) #'#f))])
              (for-clause-syntax-protect
               #'[(id ...)
                  (:do-in
                   ;;outer bindings
                   ([(outer-id ...) outer-rhs] ... ...)
                   ;; outer check
                   (and outer-check ...)
                   ;; loop bindings
                   ([loop-id loop-expr] ... ...)
                   ;; pos check
                   (and pos-guard ...)
                   ;; inner bindings
                   ([(inner-id ... ... ... id ... ok)
                     (let-values ([(inner-id ...) inner-rhs] ... ...)
                       (cond
                         [(and pre-guard ...)
                          (let-values ([(id ...) (f temp-id ...)])
                            (values inner-id ... ... ... id ... #t))]
                         [else
                          (values false* ... #f)]))])
                   ;; pre guard
                   ok
                   ;; post guard
                   (and post-guard ...)
                   ;; loop args
                   (loop-arg ... ...))]))]
           [else (raise-syntax-error #f "bad syntax" #'estx)]
           ))]
      [_ #f])))
