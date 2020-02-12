#lang racket

(require (for-syntax racket/syntax))

(provide fast-sequence-map)

(define-sequence-syntax fast-sequence-map
  (lambda () #'sequence-map)
  (lambda (stx)
    (syntax-case stx ()
      [[(id ...) (_ f seq-expr)]
       (let ([temp-ids (generate-temporaries '(temp-id))])
         (with-syntax* ([temp-id (car temp-ids)]
                        [estx (expand-for-clause stx #'[(temp-id) seq-expr])])
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
              (for-clause-syntax-protect
               #'[(id ...)
                  (:do-in
                   ;;outer bindings
                   ([(outer-id ...) outer-rhs] ...)
                   ;; outer check
                   outer-check
                   ;; loop bindings
                   ([loop-id loop-expr] ...)
                   ;; pos check
                   pos-guard
                   ;; inner bindings
                   ([(inner-id ... ... id ...)
                     (let*-values ([(inner-id ... ...)
                                    (let-values ([(inner-id ...) inner-rhs] ...)
                                      (values inner-id ... ...))]
                                   [(id ...) (f temp-id)])
                       (values inner-id ... ... id ...))])
                   ;; pre guard
                   pre-guard
                   ;; post guard
                   post-guard
                   ;; loop args
                   (loop-arg ...))])]
             [else (raise-syntax-error #f "bad :do-in clause" #'estx #'seq-expr)])))]
      [_ #f])))

(define (f x)
  (* x x))
(define xs (list 1 2 3))

#;(for ([(x) (fast-sequence-map f xs)])
  (println x))

#;(fast-sequence-map (lambda (x) (values x x)) xs)

(define (3values x) (values x x x))

#;(for ([(x y z)
       (fast-sequence-map 3values (in-range 10))])
  (printf "x y z = ~s ~s ~s\n" x y z))

;(define (0values x) (values))

#;(for ([()
       (fast-sequence-map 0values (in-range 10))])
  (printf "0 values\n"))

#;(for ([(temp-id y z)
       (fast-sequence-map 3values (in-range 10))])
  (printf "x y z = ~s ~s ~s\n" temp-id y z))
