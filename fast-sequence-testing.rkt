#lang racket

(require (for-syntax racket/syntax
                     racket
                     syntax/parse
                     syntax/srcloc)
         rackunit
         "fast-sequence-filter.rkt"
         "fast-sequence-map.rkt")

(provide exp-for-clause
         test-once
         check-fast-seq-combinators
         (all-from-out "fast-sequence-filter.rkt")
         (all-from-out "fast-sequence-map.rkt"))

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
       (fast-sequence-filter-transformer #'((id ...) f seq-expr)
                                         (seq-check-once #'(key [(id ...) seq-expr])))]
      [_ #f])))

(begin-for-syntax
  (define (rewrite-to-slow* stx)
    (syntax-case stx (fast-sequence-filter fast-sequence-map)
      [(fast-sequence-filter f seq-expr)
       #`(sequence-filter f #,(rewrite-to-slow* #'seq-expr))]
      [(fast-sequence-map f seq-expr)
       #`(sequence-map f #,(rewrite-to-slow* #'seq-expr))]
      [seq-expr
       #'seq-expr]))

  (define (rewrite-to-check-once* stx)
    (syntax-case stx (fast-sequence-filter)
      [(key (fast-sequence-filter f seq-expr))
       #`(fast-sequence-filter-check-once
          key f #,(rewrite-to-check-once* #'((key 1) seq-expr)))]
      [(key seq-expr)
       #'seq-expr]))

  (define (rewrite-to-slow stx)
    (syntax-case stx ()
      [([(id ...) seq-expr] ...)
       (with-syntax ([(seq-expr* ...) (map rewrite-to-slow*
                                           (syntax->list #'(seq-expr ...)))])
         #'([(id ...) seq-expr*] ...))]))

  (define (rewrite-to-check-once stx)
    (syntax-case stx ()
      [((key ...) [(id ...) seq-expr] ...)
       (with-syntax ([(seq-expr* ...) (map rewrite-to-check-once*
                                           (syntax->list #'((key seq-expr) ...)))])
         #'([(id ...) seq-expr*] ...))])))

(require rackunit)

(define-syntax (check-fast-seq-combinators* stx)
  (syntax-parse stx
    [(_ test-key [(id ...) seq-expr]
        ...
        (~optional (~seq #:when guard-expr)))
     (with-syntax ([msg (string-append "Test failed at " (source-location->string stx))]
                   [(key ...) (make-keys #'(seq-expr ...))])
       #`(check-equal?
          (for/list (#,@(rewrite-to-check-once #'(('(test-key key) ...) [(id ...) seq-expr] ...))
                     (~? (~@ #:when guard-expr) (~@)))
            (list id ... ...))
          (for/list (#,@(rewrite-to-slow #'([(id ...) seq-expr] ...))
                     (~? (~@ #:when guard-expr) (~@)))
            (list id ... ...))
          msg))]))

(define-syntax check-fast-seq-combinators
  (syntax-rules ()))

(begin-for-syntax
  (define-syntax-class fast-seq-test
    #:literals (check-fast-seq-combinators)
    (pattern (check-fast-seq-combinators for-clause ...))))

(define-syntax (test-once stx)
  (syntax-parse stx
    [(_ test:fast-seq-test ...)
     (with-syntax ([(key ...) (make-keys #'(test ...))])
       #'(begin
           (check-fast-seq-combinators* key test.for-clause ...) ...))]))