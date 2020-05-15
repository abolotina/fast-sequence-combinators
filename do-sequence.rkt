#lang racket

(require (for-syntax racket/syntax
                     racket
                     syntax/parse))

(provide do/sequence
         (for-syntax bind-clause
                     when-clause))

(define-sequence-syntax in-nullary-relation
  (lambda () #'in-nullary-relation/proc)
  (lambda (stx)
    (syntax-case stx ()
      [[(id ...) (_ expr)]
       (for-clause-syntax-protect
        #'[(id ...) (:do-in () #t () #t () expr #f ())])]
      [_ #f])))

(define (in-nullary-relation/proc expr) '())

(begin-for-syntax
  (define-syntax-class expanded-clause-record
    (pattern [([(outer-id ...) outer-rhs] ...)
              outer-check
              ([loop-id loop-expr] ...)
              pos-guard
              ([(inner-id ...) inner-rhs] ...)
              pre-guard
              post-guard
              (loop-arg ...)]))

  (define-syntax-class bind-clause
    (pattern [(id1:id ...) seq:expr]
             #:attr expanded (delay (expand-for-clause (current-syntax-context) this-syntax)))
    (pattern [id:id seq:expr]
             #:with (id1 ...) #'(id)
             #:attr expanded (delay (expand-for-clause (current-syntax-context) #'[(id) seq]))))

  (define-splicing-syntax-class bind-chunk
    (pattern (~seq b:bind-clause ...+)
             #:with (id ...) #'(b.id1 ... ...)
             #:attr expanded (attribute b.expanded)))

  (define-splicing-syntax-class when-clause
    (pattern (~seq #:when guard-expr:expr)))

  (define-splicing-syntax-class when-chunk
    (pattern (~seq w:when-clause ...+)
             #:with expr #'(and w.guard-expr ...))))

(define-sequence-syntax do/sequence2
  (lambda () #'in-nullary-relation/proc)
  (lambda (stx)
    (syntax-parse stx
      [[(id:id ...) (_ (b-clause:bind-clause ...+) seq-expr:expr)]
       #:with (eb:expanded-clause-record ...) #'(b-clause.expanded ...)
       #:with eb-i:expanded-clause-record (expand-for-clause stx #'[(id ...) seq-expr])
       (with-syntax* ([(loop-id* ...) (generate-temporaries #'(eb.loop-id ... ...))]
                      [(loop-arg-id ...) (generate-temporaries #'(eb.loop-arg ... ...))]
                      [(loop-arg* ...) #'(((loop-arg-id eb.loop-id ... ...) eb.inner-id ... ... ...) ...)]
                      [(loop-arg** ...) (syntax->list #'(eb.loop-arg ... ...))]
                      [(false* ...) (build-list
                                     (length (syntax->list #'(eb-i.loop-id ... eb.loop-id ... ...)))
                                     (lambda (x) #'#f))]
                      [(id-false ...) (build-list
                                       (length (syntax->list #'(id ...)))
                                       (lambda (x) #'#f))]
                      [(i-outer-id-id1-false ...) (build-list
                                                   (length (syntax->list #'(b-clause.id1 ... ... eb-i.outer-id ... ...)))
                                                   (lambda (x) #'#f))]
                      [(i-outer-check-id pos-guard-id i-pos-guard-id i-pos-guard* ids-ok
                                         process-outer-seqs)
                       (generate-temporaries #'(i-outer-check-id pos-guard-id i-pos-guard-id i-pos-guard* ids1-ok
                                                                 process-outer-seqs))]
                      [pos-guard-id* #'((pos-guard-id eb.outer-id ... ... ...) eb.loop-id ... ...)]
                      [(empty ...) (build-list
                                    (length (syntax->list #'(eb-i.loop-id ...)))
                                    (lambda (x) #''()))]
                      [(i-loop-id* ...) (generate-temporaries #'(eb-i.loop-id ...))]
                      [(i-loop-id** ...) (generate-temporaries #'(eb-i.loop-id ...))]
                      [(i-loop-id*** ...) (generate-temporaries #'(eb-i.loop-id ...))]
                      [(i-loop-arg-id ...) (generate-temporaries #'(eb-i.loop-arg ...))]
                      [(i-loop-arg* ...)
                       #'((((i-loop-arg-id eb-i.outer-id ... ...) eb-i.loop-id ...) eb-i.inner-id ... ...) ...)]
                      [(i-loop-arg** ...) (syntax->list #'(eb-i.loop-arg ...))]
                      [i-outer-check* #'(i-outer-check-id eb-i.outer-id ... ...)]
                      [(loop*) (generate-temporaries #'(loop*))]
                      [(body*) (generate-temporaries #'(body*))]
                      [(b-clause-id* ...) (generate-temporaries #'(b-clause.id1 ... ...))]
                      [(i-outer-id* ...) (generate-temporaries #'(eb-i.outer-id ... ...))]
                      [(post-guard* i-post-guard*) (generate-temporaries #'(post-guard* i-post-guard*))])
         (for-clause-syntax-protect
          #'[(id ...)
             (:do-in
              ;; outer bindings
              ([(eb.outer-id ... ... ... pos-guard-id i-outer-check-id
                             i-loop-arg-id ... process-outer-seqs)
                (let-values ([(eb.outer-id ...) eb.outer-rhs] ... ...
                             [(pos-guard-id) (lambda (eb.outer-id ... ... ...)
                                               (lambda (eb.loop-id ... ...) (and eb.pos-guard ...)))]
                             [(i-outer-check-id) (lambda (eb-i.outer-id ... ...) eb-i.outer-check)]
                             [(i-loop-arg-id) (lambda (eb-i.outer-id ... ...)
                                                (lambda (eb-i.loop-id ...)
                                                  (lambda (eb-i.inner-id ... ...) i-loop-arg**)))] ...)
                  (let ([process-outer-seqs
                         (lambda (eb.outer-id ... ... ...)
                           (lambda (eb.loop-id ... ... loop* loop-arg-id ... post-guard*)
                             (if pos-guard-id*
                                 (let-values ([(eb.inner-id ...) eb.inner-rhs] ... ...)
                                   (if (and eb.pre-guard ...
                                            post-guard*)
                                       (let-values ([(eb-i.outer-id ...) eb-i.outer-rhs] ...)
                                         i-outer-check*
                                         (loop* loop-arg* ... eb-i.loop-expr ...
                                                b-clause.id1 ... ... eb-i.outer-id ... ... #t (and eb.post-guard ...) #t))
                                       (values false* ... #f id-false ... i-outer-id-id1-false ... #f #f #f)))
                                 (values false* ... #f id-false ... i-outer-id-id1-false ... #f #f #f))))])
                    (values eb.outer-id ... ... ... pos-guard-id i-outer-check-id
                            i-loop-arg-id ... process-outer-seqs)))])
              ;; outer check
              (and eb.outer-check ...)
              ;; loop bindings
              ([eb.loop-id eb.loop-expr] ... ...
               [i-loop-id*** '()] ...
               [b-clause-id* #f] ...
               [i-outer-id* #f] ...
               [ids-ok #f]
               [post-guard* #t]
               [i-post-guard* #t])
              ;; pos check
              #t
              ;; inner bindings
              ([(i-loop-id** ... loop-id* ... ok id ... b-clause-id* ... i-outer-id* ... ids-ok post-guard* i-post-guard*)
                (let ([loop-arg-id (lambda (eb.loop-id ... ...) (lambda (eb.inner-id ... ... ...) loop-arg**))] ...)
                  (let loop ([eb.loop-id eb.loop-id] ... ...
                             [i-loop-id*** i-loop-id***] ...
                             [b-clause-id* b-clause-id*] ...
                             [i-outer-id* i-outer-id*] ...
                             [ids-ok ids-ok]
                             [post-guard* post-guard*]
                             [i-post-guard* i-post-guard*])
                    (cond
                      [ids-ok
                       (let-values ([(eb-i.outer-id ... ...) (values i-outer-id* ...)])
                         (let-values ([(eb-i.loop-id ...) (values i-loop-id*** ...)]) ;
                           (cond
                             [eb-i.pos-guard
                              (let-values ([(eb-i.inner-id ...) eb-i.inner-rhs] ...)
                                (if (and eb-i.pre-guard
                                         i-post-guard*)
                                    (values i-loop-arg* ... eb.loop-id ... ... #t
                                            id ... b-clause-id* ... i-outer-id* ... ids-ok
                                            post-guard* eb-i.post-guard)
                                    ((process-outer-seqs eb.outer-id ... ... ...)
                                     eb.loop-id ... ... loop loop-arg-id ... post-guard*)))]
                             [else
                              ((process-outer-seqs eb.outer-id ... ... ...)
                               eb.loop-id ... ... loop loop-arg-id ... post-guard*)])))]
                      [else
                       ((process-outer-seqs eb.outer-id ... ... ...)
                        eb.loop-id ... ... loop loop-arg-id ... post-guard*)])))])
              ;; pre guard
              ok
              ;; post guard
              ok
              ;; loop args
              (loop-id* ... i-loop-id** ... b-clause-id* ... i-outer-id* ... ids-ok post-guard* i-post-guard*))]))]
      [_ #f])))

(define-sequence-syntax do/sequence
  (lambda () #'for/list)
  (lambda (stx)
    (syntax-parse stx
      [[(id:id ...) (_ () body:expr ...+)]
       (for-clause-syntax-protect
        #'[(id ...) (:do-in ([(id ...) body ...]) #t () #t () #t #f ())])]
      [[(id:id ...) (_ (w:when-chunk . rest) body:expr ...+)]
       (for-clause-syntax-protect
        #'[(id ...) (do/sequence2 ([() (in-nullary-relation w.expr)]) (do/sequence rest body ...))])]
      [[(id:id ...) (_ (b:bind-chunk . rest) body:expr ...+)]
       (for-clause-syntax-protect
        #'[(id ...) (do/sequence2 (b.b ...) (do/sequence rest body ...))])]
      [_ #f])))

;; ----------

#;(for ([() (do/sequence () 1)])
  (println 2))

#;(for ([(x) (do/sequence (#:when #t) 1)])
  (println x))

#;(for ([(x) (do/sequence (#:when #f) 1)])
  (println x))

#;(for ([x (do/sequence ([(x) (in-list '(1 2 3 4 5))] #:when (odd? x)) x)])
  (println x))

#;(for ([(x y) (do/sequence (#:when #t
                           [(x) (in-list '(1 2 3 4 5))]
                           [(y) (in-list '(a b c d e))])
             (values x y))])
  (println (list x y)))

#;(for ([(x y) (do/sequence (#:when #t
                           [x (in-list '(1 2 3 4 5))]
                           [y (in-list '(#\A #\B #\c #\d #\e))]
                           #:when (odd? x)
                           #:when (char-upper-case? y))
             (values x y))])
  (println (list x y)))

#;(for ([(x) (do/sequence (#:when #t #:when (odd? 2) [(x*) (in-list '(1 2 3 4 5))]) x*)])
  (println x))

#;(for ([(a b) (do/sequence ([(a) (in-list '(1 2 3 4 5))]
                           [(z) (in-list '(a b c d e))]
                           #:when (odd? a))
               (values a z))])
  (println (list a b)))

#;(for ([(a b) (do/sequence ([(y) (in-list '(1 2 3 4 5))] #:when (odd? y)
                           [(z) (in-list '(a b c d e))])
             (values y z))])
  (println (list a b)))

#;(for ([(x) (do/sequence ([(x) (in-list '((1 2) (3 4) (5 6)))]
                         #:when guard-expr
                         [(y) (in-list x)])
           y)])
  (println x))

#;(for ([(x) (do/sequence (#:when #t
                         [(x) (in-list '((1 2) (3 4) (5 6)))]
                         #:when guard-expr
                         [(y) (in-list x)])
           y)])
  (println x))

#;(for ([(x) (do/sequence (#:when #f
                         #:when #t
                         [(x) (in-list '((1 2) (3 4) (5 6)))]
                         #:when guard-expr
                         [(y) (in-list x)])
           y)])
  (println x))

#;(for ([(y) (do/sequence ([(lst) '((1 2) () (2 4) (5 6))]
                       #:when #t
                       [(x) (in-list lst)]
                       #:when (odd? x))
                      x)])
  (println y))

#;(for ([(y) (do/sequence ([(x) (do/sequence ([(seq) (in-list '((1 2) (3 4) (5 6)))]
                                            #:when guard-expr
                                            [(x*) (in-list seq)])
                                           x*)])
                        x)])
  (println y))

#;(for ([(y) (do/sequence ([(outer-lst1) (in-list '(((1 2) (3 7) () (2 4) (5 6))
                                                  ((1 2) (2 4))
                                                  ((1 2) (3 4))))]
                         [(outer-lst2) (in-list '(((1 2) (3 7) () (2 4) (5 6))
                                                  ((1 2) (2 4))
                                                  ((1 2) (3 4))))]
                         #:when (odd? (caadr outer-lst1))
                         [(inner-lst) (in-list outer-lst1)]
                         #:when (and (pair? inner-lst) (odd? (car inner-lst)))
                         [(x) (in-list inner-lst)]
                         #:when (odd? x))
             (list x outer-lst2))])
  (println y))

#;(for ([(y) (do/sequence ([(outer-lst1) (in-list '(((1 2) (3 7) () (2 4) (5 6))
                                                  ((1 2) (2 4))
                                                  ((1 2) (3 4))))]
                         [(outer-lst2) (in-list '(((1 2) (3 7) () (2 4) (5 6))
                                                  ((1 2) (2 4))
                                                  ((1 2) (3 4))))]
                         #:when (odd? (caadr outer-lst1))
                         [(inner-lst x) (do/sequence ([(inner-lst) (in-list outer-lst1)]
                                            #:when (and (pair? inner-lst) (odd? (car inner-lst)))
                                            [(x) (in-list inner-lst)]
                                            #:when (odd? x))
                                (values inner-lst x))])
             (list x outer-lst2))])
  (println y))

#;(for ([(y) (do/sequence ([(outer-lst1) (in-list '(((1 2) (3 7) () (2 4) (5 6))
                                                  ((1 2) (2 4))
                                                  ((1 2) (3 4))))]
                         [(outer-lst2) (in-list '(((1 2) (3 7) () (2 4) (5 6))
                                                  ((1 2) (2 4))
                                                  ((1 2) (3 4))))]
                         #:when (odd? (caadr outer-lst1))
                         [(inner-lst) (in-list outer-lst1)]
                         #:when (and (pair? inner-lst) (odd? (car inner-lst)))
                         [(x) (in-list inner-lst)]
                         #:when (odd? x)
                         [z (in-value x)])
             (list x z outer-lst2))])
  (println y))

#;(for ([(y) (do/sequence ([(outer-lst1) (in-list '(((1 2) (3 7) () (2 4) (5 6))
                                                  ((1 2) (2 4))
                                                  ((1 2) (3 4))))]
                         [(outer-lst2) (in-list '(((1 2) (3 7) () (2 4) (5 6))
                                                  ((1 2) (2 4))
                                                  ((1 2) (3 4))))]
                         #:when (odd? (caadr outer-lst1))
                         [(inner-lst x z) (do/sequence ([(inner-lst) (in-list outer-lst1)]
                                                      #:when (and (pair? inner-lst) (odd? (car inner-lst)))
                                                      [(x z) (do/sequence ([(x) (in-list inner-lst)]
                                                                           #:when (odd? x)
                                                                           [(z) (in-value x)])
                                                               (values x z))])
                                          (values inner-lst x z))])
             (list x z outer-lst2))])
  (println y))

#;(for ([(x) (do/sequence ([(z) (in-value 1)]) z)])
  (println x))
