#lang racket

(require (for-syntax racket/syntax)
         syntax/unsafe/for-transform)

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

(define (f x)
  (* x x))
(define xs (list 1 2 3))

#;(for ([(x y) (fast-sequence-map values xs xs)])
    (println (list x y)))

#;(for ([(x y) (fast-sequence-map values xs (in-string "abcd"))])
    (println (list x y)))

#;(for ([(x y) (fast-sequence-map values (in-port read-char (open-input-string "a1b2c3")) xs)])
    (println (list x y)))

#;(for ([(x) (fast-sequence-map f xs)])
    (println x))

#;(fast-sequence-map (lambda (x) (values x x)) xs)

(define (3values x) (values x x x))

#;(for ([(x y z)
         (fast-sequence-map 3values (in-range 10))])
    (printf "x y z = ~s ~s ~s\n" x y z))

(define (0values x) (values))

#;(for ([()
         (fast-sequence-map 0values (in-range 10))])
    (printf "0 values\n"))

#;(for ([(temp-id y z)
         (fast-sequence-map 3values (in-range 10))])
    (printf "x y z = ~s ~s ~s\n" temp-id y z))
