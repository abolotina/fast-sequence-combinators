#lang racket

(require (for-syntax racket/syntax
                     racket
                     syntax/parse))

(provide do/sequence)

(begin-for-syntax
  (define-splicing-syntax-class do/seq-clause
    (pattern [(id:id ...) seq:expr]
             #:attr ids-being-bound (syntax->list #'(id ...)))
    (pattern (~seq #:when guard-expr:expr)
             #:attr ids-being-bound '()))

  ;; do/sequence-transformer : Syntax[[(id:id ...) (do/sequence (clause:do/seq-clause ...) body:expr ...+)]] ->
  ;;                           Syntax[ExpandedForClause]
  (define (do/sequence-transformer stx)
    (syntax-parse stx
      [[(id:id ...) (_ () body:expr ...+)]
       (for-clause-syntax-protect
        #'[(id ...) (:do-in () #t () #f () #f #f ())])]
      [[(id:id ...) (_ ((~seq #:when guard-expr:expr) ...+) body:expr ...+)]
       (for-clause-syntax-protect
        #'[(id ...)
           (:do-in ([(id ...) (begin body ...)]) #t () #t () (and guard-expr ...) #f ())])]
      [[(id:id ...) (_ ((~seq #:when guard-expr1:expr) ...
                        [(id*:id ...) seq-expr:expr] ...+
                        (~seq #:when guard-expr2:expr) ...)
                      body:expr ...+)]
       (with-syntax* ([estx (map (lambda (bind-clause)
                                   (expand-for-clause stx bind-clause))
                                 (syntax->list #'([(id* ...) seq-expr] ...)))])
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
            (with-syntax* ([(loop-id* ...) (generate-temporaries #'(loop-id ... ...))]
                           [(loop-arg-id ...) (generate-temporaries #'(loop-arg ... ...))]
                           [(loop-arg* ...) #'(((loop-arg-id loop-id ... ...) inner-id ... ... ...) ...)]
                           [(loop-arg** ...) (syntax->list #'(loop-arg ... ...))]
                           [(false* ...) (build-list
                                          (length (syntax->list #'(loop-arg* ...)))
                                          (lambda (x) #'#f))]
                           [(id-false ...) (build-list
                                            (length (syntax->list #'(id ...)))
                                            (lambda (x) #'#f))]
                           [(body*) (generate-temporaries #'(body*))]
                           [(guard) (generate-temporaries #'(guard))])
              (for-clause-syntax-protect
               #'[(id ...)
                  (:do-in
                   ;; outer bindings
                   ([(outer-id ...) outer-rhs] ... ...
                    [(body*) (lambda (id* ... ...) body ...)]
                    [(guard) (and guard-expr1 ...)])
                   ;; outer check
                   (and outer-check ...)
                   ;; loop bindings
                   ([loop-id loop-expr] ... ...)
                   ;; pos check
                   guard
                   ;; inner bindings
                   ([(loop-id* ... ok id ...)
                     (let ([loop-arg-id (lambda (loop-id ... ...) (lambda (inner-id ... ... ...) loop-arg**))] ...)
                       (let loop ([loop-id loop-id] ... ...)
                         (if (and pos-guard ...)
                             (let-values ([(inner-id ...) inner-rhs] ... ...)
                               (if (and pre-guard ...
                                        post-guard ...)
                                   (if (and guard-expr2 ...)
                                       (let-values ([(id ...) (body* id* ... ...)])
                                         (values loop-arg* ... #t id ...))
                                       (loop loop-arg* ...))
                                   (values loop-arg* ... #f id-false ...)))
                             (values false* ... #f id-false ...))))])
                   ;; pre guard
                   ok
                   ;; post guard
                   ok
                   ;; loop args
                   (loop-id* ...))]))]
           [else (raise-syntax-error #f "bad syntax" #'estx)]))]
      [[(id:id ...) (_ ((~seq #:when guard-expr:expr) ...
                        [(id1:id ...) outer-seq-expr:expr] ...+ (~seq #:when guard-expr1:expr) ...+
                        [(id2:id ...) inner-seq-expr:expr] ...+ (~seq #:when guard-expr2:expr) ...)
                      body ...+)]
       (with-syntax* ([estx1 (map (lambda (bind-clause)
                                    (expand-for-clause stx bind-clause))
                                  (syntax->list #'([(id1 ...) outer-seq-expr] ...)))]
                      [estx2 (map (lambda (bind-clause)
                                    (expand-for-clause stx bind-clause))
                                  (syntax->list #'([(id2 ...) inner-seq-expr] ...)))])
         (syntax-case #'(estx1 estx2) ()
           [(((([(outer-id ...) outer-rhs] ...)
               outer-check
               ([loop-id loop-expr] ...)
               pos-guard
               ([(inner-id ...) inner-rhs] ...)
               pre-guard
               post-guard
               (loop-arg ...)) ...)
             
             ((([(i-outer-id ...) i-outer-rhs] ...)
               i-outer-check
               ([i-loop-id i-loop-expr] ...)
               i-pos-guard
               ([(i-inner-id ...) i-inner-rhs] ...)
               i-pre-guard
               i-post-guard
               (i-loop-arg ...)) ...))
            ;; ==>
            (with-syntax* ([(loop-id* ...) (generate-temporaries #'(loop-id ... ...))]
                           [(loop-arg-id ...) (generate-temporaries #'(loop-arg ... ...))]
                           [(loop-arg* ...) #'(((loop-arg-id loop-id ... ...) inner-id ... ... ...) ...)]
                           [(loop-arg** ...) (syntax->list #'(loop-arg ... ...))]
                           [(false* ...) (build-list
                                          (length (syntax->list #'(i-loop-id ... ... loop-id ... ...)))
                                          (lambda (x) #'#f))]
                           [(id-false ...) (build-list
                                            (length (syntax->list #'(id ...)))
                                            (lambda (x) #'#f))]
                           [(i-outer-id-id1-false ...) (build-list
                                                        (length (syntax->list #'(id1 ... ... i-outer-id ... ... ...)))
                                                        (lambda (x) #'#f))]
                           [pos-guard-id* #'((pos-guard-id outer-id ... ... ...) loop-id ... ...)]

                           [(i-outer-check-id i-pos-guard-id i-pos-guard* ids-ok)
                            (generate-temporaries #'(i-outer-check-id i-pos-guard-id i-pos-guard* ids1-ok))]
                           [(empty ...) (build-list
                                         (length (syntax->list #'(i-loop-id ... ...)))
                                         (lambda (x) #''()))]
                           [(i-loop-id* ...) (generate-temporaries #'(i-loop-id ... ...))]
                           [(i-loop-id** ...) (generate-temporaries #'(i-loop-id ... ...))]
                           [(i-loop-id*** ...) (generate-temporaries #'(i-loop-id ... ...))]
                           [(i-loop-arg-id ...) (generate-temporaries #'(i-loop-arg ... ...))]
                           [(i-loop-arg* ...)
                            #'((((i-loop-arg-id i-outer-id ... ...) i-loop-id ... ...) i-inner-id ... ... ...) ...)]
                           [(i-loop-arg** ...) (syntax->list #'(i-loop-arg ... ...))]
                           [i-outer-check* #'(i-outer-check-id i-outer-id ... ... ...)]
                           [(loop*) (generate-temporaries #'(loop*))]
                           [(body*) (generate-temporaries #'(body*))]
                           [(id1* ...) (generate-temporaries #'(id1 ... ...))]
                           [(guard) (generate-temporaries #'(guard))])
              (for-clause-syntax-protect
               #'[(id ...)
                  (:do-in
                   ;; outer bindings
                   ([(outer-id ... ... ... pos-guard-id i-outer-check-id
                               i-loop-arg-id ... process-outer-seqs body* guard)
                     (let-values ([(outer-id ...) outer-rhs] ... ...
                                  [(pos-guard-id) (lambda (outer-id ... ... ...)
                                                    (lambda (loop-id ... ...) (and pos-guard ...)))]
                                  [(i-outer-check-id) (lambda (i-outer-id ... ... ...) (and i-outer-check ...))]
                                  [(i-loop-arg-id) (lambda (i-outer-id ... ...)
                                                     (lambda (i-loop-id ... ...)
                                                       (lambda (i-inner-id ... ... ...) i-loop-arg**)))] ...
                                  [(body*) (lambda (id1 ... ... id2 ... ...) body ...)]
                                  [(guard) (and guard-expr ...)])
                       (let ([process-outer-seqs
                              (lambda (outer-id ... ... ...)
                                (lambda (loop-id ... ... loop* loop-arg-id ...)
                                  (if pos-guard-id*
                                      (let-values ([(inner-id ...) inner-rhs] ... ...)
                                        (if (and pre-guard ...
                                                 post-guard ...)
                                            (if (and guard-expr1 ...)
                                                (let-values ([(i-outer-id ...) i-outer-rhs] ... ...)
                                                  i-outer-check*
                                                  (loop* loop-arg* ... i-loop-expr ... ...
                                                         id1 ... ... i-outer-id ... ... ... #t))
                                                (loop* loop-arg* ... empty ... i-outer-id-id1-false ... #f))
                                            (values false* ... #f id-false ... i-outer-id-id1-false ... #f)))
                                      (values false* ... #f id-false ... i-outer-id-id1-false ... #f))))])
                         (values outer-id ... ... ... pos-guard-id i-outer-check-id
                                 i-loop-arg-id ... process-outer-seqs body* guard)))])
                   ;; outer check
                   (and outer-check ...)
                   ;; loop bindings
                   ([loop-id loop-expr] ... ...
                    [i-loop-id*** '()] ...
                    [id1* #f] ...
                    [i-outer-id #f] ... ... ...
                    [ids-ok #f])
                   ;; pos check
                   guard
                   ;; inner bindings
                   ([(i-loop-id** ... loop-id* ... ok id ... id1* ... i-outer-id ... ... ... ids-ok)
                     (let ([loop-arg-id (lambda (loop-id ... ...) (lambda (inner-id ... ... ...) loop-arg**))] ...)
                       (let loop ([loop-id loop-id] ... ...
                                  [i-loop-id*** i-loop-id***] ...
                                  [id1* id1*] ...
                                  [i-outer-id i-outer-id] ... ... ...
                                  [ids-ok ids-ok])
                         (cond
                           [ids-ok
                            (let-values ([(i-loop-id ... ...) (values i-loop-id*** ...)])
                              (cond
                                [(and i-pos-guard ...)
                                 (let-values ([(i-inner-id ...) i-inner-rhs] ... ...)
                                   (if (and i-pre-guard ...
                                            i-post-guard ...)
                                       (if (and guard-expr2 ...)
                                           (let-values ([(id ...) (body* id1* ... id2 ... ...)])
                                             (values i-loop-arg* ... loop-id ... ... #t
                                                     id ... id1* ... i-outer-id ... ... ... ids-ok))
                                           (loop loop-id ... ... i-loop-arg* ... id1* ... i-outer-id ... ... ... #t))
                                       ((process-outer-seqs outer-id ... ... ...) loop-id ... ... loop loop-arg-id ...)))]
                                [else
                                 ((process-outer-seqs outer-id ... ... ...) loop-id ... ... loop loop-arg-id ...)]))]
                           [else
                            ((process-outer-seqs outer-id ... ... ...) loop-id ... ... loop loop-arg-id ...)])))])
                   ;; pre guard
                   ok
                   ;; post guard
                   ok
                   ;; loop args
                   (loop-id* ... i-loop-id** ... id1* ... i-outer-id ... ... ... ids-ok))]))]
           [else (raise-syntax-error #f "bad syntax" #'(estx1 estx2))]))]
      [[(id:id ...) (_ ([(id*:id ...) seq-expr:expr] ...
                        (~seq #:when guard-expr:expr) ...
                        clause:do/seq-clause ...)
                      body:expr ...+)]
       (do/sequence-transformer
        (with-syntax ([(id** ...) (for*/list ([ids (in-list (attribute clause.ids-being-bound))]
                                              [id  (in-list ids)])
                                    id)])
          #'[(id ...) (do/sequence ([(id* ...) seq-expr] ...
                                    #:when guard-expr ...
                                    [(id** ...) (do/sequence (clause ...)
                                                             (values id** ...))])
                                   body ...)]))]
      [_ #f])))

(define-sequence-syntax do/sequence
  (lambda () #'for/list)
  (lambda (stx) (do/sequence-transformer stx)))

#;(define-sequence-syntax do/sequence*
  (lambda () #'for/list)
  (lambda (stx) (do/sequence-transformer stx)))

#;(define-syntax (do/sequence stx)
  (syntax-parse stx
    [(_ ([(id1:id ...) seq-expr1:expr] ...+
         (~seq #:when guard-expr1:expr) ...+
         [(id2:id ...) seq-expr2:expr] ...+
         (~seq #:when guard-expr2:expr) ...+
         clause:do/seq-clause ...+)
       body:expr ...+)
     (with-syntax ([(id** ...) (for*/list ([ids (in-list (attribute clause.ids-being-bound))]
                                           [id  (in-list ids)])
                                 id)])
       #'(do/sequence* ([(id1 ...) seq-expr1] ...
                        #:when guard-expr1 ...
                        [(id2 ... ... id** ...) (do/sequence* ([(id2 ...) seq-expr2] ...
                                                               #:when guard-expr2 ...
                                                               clause ...)
                                                  (values id2 ... ... id** ...))])
           body ...))]
    [(_ (clause:do/seq-clause ...+)
       body:expr ...+)
     #'(do/sequence* (clause ...)
         body ...)]))

;; ----------

#;(for ([() (do/sequence () 1)])
  (println 2))

#;(for ([(x) (do/sequence (#:when #t) 1)])
  (println x))

#;(for ([(x) (do/sequence (#:when #f) 1)])
  (println x))

#;(for ([(x) (do/sequence ([(x) (in-list '(1 2 3 4 5))] #:when (odd? x)) x)])
  (println x))

#;(for ([(x y) (do/sequence (#:when #t
                           [(x) (in-list '(1 2 3 4 5))]
                           [(y) (in-list '(a b c d e))])
             (values x y))])
  (println (list x y)))

#;(for ([(x y) (do/sequence (#:when #t
                           [(x) (in-list '(1 2 3 4 5))]
                           [(y) (in-list '(#\A #\B #\c #\d #\e))]
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

#;(for ([x (do/sequence ([(outer-lst1) (in-list '(((1 2) (3 7) () (2 4) (5 6))
                                                ((1 2) (2 4))
                                                ((1 2) (3 4) (6 8) (4 2))))]
                       [(outer-lst2) (in-list '(((1 2) (3 7) () (2 4) (5 6))
                                                ((1 2) (9 4))
                                                ((1 2) (3 4) (8 6) (7 2))))]
                       #:when (odd? (caadr outer-lst1))
                       #:when (odd? (caadr outer-lst2))
                       [(inner-lst1) (in-list outer-lst1)]
                       [(inner-lst2) (in-list outer-lst2)]
                       #:when (and (pair? inner-lst1) (even? (car inner-lst1)))
                       #:when (and (pair? inner-lst2) (even? (car inner-lst2))))
           (list inner-lst1 inner-lst2))])
  (println x))

#;(for ([(x outer-lst2) (do/sequence ([(outer-lst1) (in-list '(((1 2) (3 7) () (2 4) (5 6))
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
                                   (values x outer-lst2))])
  (println (list x outer-lst2)))
