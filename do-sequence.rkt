#lang racket

(require (for-syntax racket/syntax
                     racket
                     syntax/parse
                     syntax/stx
                     syntax/unsafe/for-transform
                     #;macro-debugger/emit))

(provide do/sequence
         do/sequence2
         in-nullary-relation
         in-protect
         in-merge
         (for-syntax bind-clause
                     when-clause
                     when-chunk
                     bind-chunk
                     expanded-clause-record
                     nest))

;; A helper sequence that contains/represents information
;; about a number of iterations: 1 or 0.
;;
;;   (in-nullary-relation #t) = [(values)]
;;   (in-nullary-relation #f) = []
(define-sequence-syntax in-nullary-relation
  (lambda () #'in-nullary-relation/proc)
  (lambda (stx)
    (syntax-case stx ()
      [[(id ...) (_ expr)]
       (for-clause-syntax-protect
        (with-syntax ([(expr*) (generate-temporaries #'(expr))])
          #'[(id ...) (:do-in ([(expr*) (lambda () expr)]) #t () #t () (expr*) #f ()) ; workaround for a bug in Racket
                      #;(:do-in () #t () #t () expr #f ())]))] ; original implementation
      [_ #f])))

;;ryan: The procedure version is wrong. But see the comment far below
;;  about do/sequence and for/list.
(define (in-nullary-relation/proc expr) '())

;; An expanded clause record. Contains information to fill in the loop
;; skeleton: how to start looping, how to decide whether to stop,
;; how to bind loop variables, how to recur, and a few more checks. 
(begin-for-syntax
  (define-syntax-class expanded-clause-record
    (pattern [([(outer-id ...) outer-rhs] ...)
              outer-check
              ([loop-id loop-expr] ...)
              pos-guard
              ([(inner-id ...) inner-rhs] ...)
              pre-guard
              post-guard
              (loop-arg ...)]
             #:attr protected (delay (protect-bindings this-syntax))))
  
  ;; A binding clause.
  ;;
  ;; bind-clause =
  ;;   | [(id ...) seq-expr]
  ;;   | [id seq-expr]
  (define-syntax-class bind-clause
    (pattern [(id1:id ...) seq:expr]
             #:attr expanded (delay (expand-for-clause (current-syntax-context) this-syntax)))
    (pattern [id:id seq:expr]
             #:with (id1 ...) #'(id)
             #:attr expanded (delay (expand-for-clause (current-syntax-context) #'[(id) seq]))))

  ;; A chunk of binding clauses
  (define-splicing-syntax-class bind-chunk
    (pattern (~seq b:bind-clause ...+)
             #:with (id ...) #'(b.id1 ... ...)
             #:attr expanded (attribute b.expanded)))

  ;; A when-clause.
  ;;
  ;; when-clause =
  ;;   | #:when when-guard
  (define-splicing-syntax-class when-clause
    (pattern (~seq #:when guard-expr:expr)))

  ;; A chunk of when-clauses.
  (define-splicing-syntax-class when-chunk
    (pattern (~seq w:when-clause ...+)
             #:with expr #'(and w.guard-expr ...))))

(begin-for-syntax
  ;; Listof^0 X = X
  ;; Listof^N+1 X = Listof (Listof^N X)

  ;; (gen-temp-tree 2 ((a b) (c))) = (list (list #'a #'b) (list #'c))
  
  ;; gen-temp-tree : Integer (StxListof^N Any) -> (Listof^N Identifier)
  (define (gen-temp-tree n xs)
    (cond
      [(zero? n) (car (generate-temporaries xs))]
      [(= n 1) (generate-temporaries xs)]
      [else (map (lambda (x) (gen-temp-tree (sub1 n) x)) (stx->list xs))]))

  ;; protect-bindings : Syntax[ECR[G][G']] -> Syntax[PECR[G][G']]
  (define (protect-bindings stx)
    (syntax-parse stx
      [ecr:expanded-clause-record
       (with-syntax ([(ecr-loop-expr* ...) (generate-temporaries #'(ecr.loop-id ...))]
                     [(ecr-outer-check*) (generate-temporaries #'(outer-check))]
                     [(ecr-pos-guard*) (generate-temporaries #'(pos-guard))]
                     [(ecr-inner-rhs* ...) (generate-temporaries
                                            (map (lambda (x) 'inner-rhs) (syntax->list #'(ecr.inner-rhs ...))))]
                     [(ecr-pre-guard*) (generate-temporaries #'(pre-guard))]
                     [(ecr-post-guard*) (generate-temporaries #'(post-guard))]
                     [(ecr-loop-arg* ...) (generate-temporaries #'(ecr.loop-id ...))]
                     [(loop-id* ...) (generate-temporaries #'(ecr.loop-id ...))]
                     [((inner-id* ...) ...) (gen-temp-tree 2 #'((ecr.inner-id ...) ...))])
         #'(;; outer bindings
            ([(ecr.outer-id ...) ecr.outer-rhs] ...
             [(ecr-outer-check*) (lambda (ecr.outer-id ... ...) ecr.outer-check)]
             [(ecr-loop-expr*) (lambda (ecr.outer-id ... ...) ecr.loop-expr)] ...
             [(ecr-pos-guard*) (lambda (ecr.outer-id ... ... loop-id* ...)
                                 (let ([ecr.loop-id loop-id*] ...) ecr.pos-guard))]
             [(ecr-inner-rhs*) (lambda (ecr.outer-id ... ... loop-id* ...)
                                 (let ([ecr.loop-id loop-id*] ...) ecr.inner-rhs))] ...
             [(ecr-pre-guard*) (lambda (ecr.outer-id ... ... loop-id* ... inner-id* ... ...)
                                 (let ([ecr.loop-id loop-id*] ...)
                                   (let ([ecr.inner-id inner-id*] ... ...) ecr.pre-guard)))]
             [(ecr-post-guard*) (lambda (ecr.outer-id ... ... loop-id* ... inner-id* ... ...)
                                 (let ([ecr.loop-id loop-id*] ...)
                                   (let ([ecr.inner-id inner-id*] ... ...) ecr.post-guard)))]
             [(ecr-loop-arg*) (lambda (ecr.outer-id ... ... loop-id* ... inner-id* ... ...)
                                 (let ([ecr.loop-id loop-id*] ...)
                                   (let ([ecr.inner-id inner-id*] ... ...) ecr.loop-arg)))] ...)
            ;; outer check
            (ecr-outer-check* ecr.outer-id ... ...)
            ;; loop bindings
            ([ecr.loop-id (ecr-loop-expr* ecr.outer-id ... ...)] ...)
            ;; pos check
            (ecr-pos-guard* ecr.outer-id ... ...
                            ecr.loop-id ...)
            ;; inner bindings
            ([(ecr.inner-id ...) (ecr-inner-rhs* ecr.outer-id ... ...
                                                 ecr.loop-id ...)] ...)
            ;; pre guard
            (ecr-pre-guard* ecr.outer-id ... ...
                            ecr.loop-id ...
                            ecr.inner-id ... ...)
            ;; post guard
            (ecr-post-guard* ecr.outer-id ... ...
                             ecr.loop-id ...
                             ecr.inner-id ... ...)
            ;; loop args
            ((ecr-loop-arg* ecr.outer-id ... ...
                            ecr.loop-id ...
                            ecr.inner-id ... ...) ...)))]))
  
  (define (merge stx)
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
          (ecr.loop-arg ... ...))]))

  #;(define (merge stx)
    (define result (merge* stx))
    #;(emit-remark "merging" stx "produced" result)
    #;(emit-local-step stx result #:id #'for)
    result))

(define-sequence-syntax in-protect
  (lambda (stx)
    (raise-syntax-error #f "only allowed in a fast sequence context" stx))
  (lambda (stx)
    (syntax-parse stx
      [[(id:id ...) (_ seq-expr:expr)]
       #:with ecr:expanded-clause-record (expand-for-clause stx #'[(id ...) seq-expr])
       #:with pecr:expanded-clause-record #'ecr.protected
       (for-clause-syntax-protect
        #'[(id ...)
           (:do-in
            ([(pecr.outer-id ...) pecr.outer-rhs] ...)
            pecr.outer-check
            ([pecr.loop-id pecr.loop-expr] ...)
            pecr.pos-guard
            ([(pecr.inner-id ...) pecr.inner-rhs] ...)
            pecr.pre-guard
            pecr.post-guard
            (pecr.loop-arg ...))])])))

;; Elements of all seq-expr's must be single-valued. Behaves like in-parallel.
(define-sequence-syntax in-merge
  (lambda (stx)
    (raise-syntax-error #f "only allowed in a fast sequence context" stx))
  (lambda (stx)
    (syntax-parse stx
      [[(id:id ...) (_ seq-expr:expr ...)]
       #:with (b-clause:bind-clause ...) #'([(id) seq-expr] ...)
       #:with (ecr:expanded-clause-record ...) #'(b-clause.expanded ...)
       #:with mecr:expanded-clause-record (merge #'(ecr ...))
       (for-clause-syntax-protect
        #'[(id ...)
           (:do-in
            ([(mecr.outer-id ...) mecr.outer-rhs] ...)
            mecr.outer-check
            ([mecr.loop-id mecr.loop-expr] ...)
            mecr.pos-guard
            ([(mecr.inner-id ...) mecr.inner-rhs] ...)
            mecr.pre-guard
            mecr.post-guard
            (mecr.loop-arg ...))])])))

(define-syntax (let/ribs stx)
  (syntax-parse stx
    [(let/ribs [([id1 expr1] ...) ([id2 expr2] ...) ...+] body ...+)
     #'(let ([id1 expr1] ...)
         (let/ribs [([id2 expr2] ...) ...] body ...))]
    [(let/ribs [([id expr] ...)] body ...+)
     #'(let ([id expr] ...)
         body ...)]))

(define-syntax (lambda/ribs stx)
  (syntax-parse stx
    [(_ [(arg ...) ...+] body ...+)
     (with-syntax ([((arg* ...) ...) (gen-temp-tree 2 #'((arg ...) ...))])
       #'(lambda (arg* ... ...)
           (let/ribs [([arg arg*] ...) ...] body ...)))]))

(begin-for-syntax
  ;; nest : Syntax[((Id ...) ExpandedClauseRecord ExpandedClauseRecord)] -> Syntax[ExpandedClauseRecord]
  (define (nest stx)
    (syntax-parse stx
      [((id:id ...)
        eb:expanded-clause-record
        eb-i:expanded-clause-record)
       (with-syntax ([(post-guard* i-post-guard* inner-is-initialized? ok)
                      (generate-temporaries #'(post-guard* i-post-guard* inner-is-initialized? ok))]
                     [(post-guard** i-post-guard** inner-is-initialized?*)
                      (generate-temporaries #'(post-guard** i-post-guard** inner-is-initialized?*))]
                     [(id-false ...) (build-list
                                       (length (syntax->list #'( eb-i.loop-id ... eb.loop-id ... eb.inner-id ... ...
                                                                eb-i.outer-id ... ... id ...)))
                                       (lambda (x) #'#f))]
                     [(loop-id* ...) (generate-temporaries #'(eb.loop-id ...))]
                     [(i-loop-id* ...) (generate-temporaries #'(eb-i.loop-id ...))]
                     [(i-loop-id** ...) (generate-temporaries #'(eb-i.loop-id ...))]
                     [(loop-arg* ...) (generate-temporaries #'(eb.loop-id ...))]
                     [((inner-id* ...) ...) (gen-temp-tree 2 #'((eb.inner-id ...) ...))]
                     [((i-outer-id* ...) ...) (gen-temp-tree 2 #'((eb-i.outer-id ...) ...))]
                     [(loop-id** ...) (generate-temporaries #'(eb.loop-id ...))]
                     [((inner-id** ...) ...) (gen-temp-tree 2 #'((eb.inner-id ...) ...))]
                     [((i-outer-id** ...) ...) (gen-temp-tree 2 #'((eb-i.outer-id ...) ...))])
         (for-clause-syntax-protect
          #'(;; outer bindings
             ([(eb.outer-id ...) eb.outer-rhs] ...)
             ;; outer check
             eb.outer-check
             ;; loop bindings
             ([loop-id* eb.loop-expr] ...
              [inner-id* #f] ... ...
              [i-loop-id* #f] ...
              [i-outer-id* #f] ... ...
              [inner-is-initialized? #f]
              [post-guard* #t]
              [i-post-guard* #t])
             ;; pos check
             #t
             ;; inner bindings
             ([(i-loop-id** ... loop-id** ... inner-id** ... ... i-outer-id** ... ...
                id ... post-guard** i-post-guard** inner-is-initialized?* ok)
               (let ()
                 (define (loop-with-inner loop-id* ... inner-id* ... ...
                                          i-outer-id* ... ... i-loop-id* ...
                                          post-guard* i-post-guard*)
                   (let ([eb.loop-id loop-id*] ...)
                     (let ([eb.inner-id inner-id*] ... ...)
                       (let ([eb-i.outer-id i-outer-id*] ... ...)
                         (let ([eb-i.loop-id i-loop-id*] ...)
                           (cond
                             [eb-i.pos-guard
                              (let-values ([(eb-i.inner-id ...) eb-i.inner-rhs] ...)
                                (if (and eb-i.pre-guard
                                         i-post-guard*)
                                    ;; Case 1
                                    (values eb-i.loop-arg ... loop-id* ... inner-id* ... ...
                                            i-outer-id* ... ... id ...
                                            post-guard* eb-i.post-guard
                                            #t #t)
                                    (loop-without-inner loop-id* ... post-guard*)))]
                             [else
                              (loop-without-inner loop-id* ... post-guard*)]))))))
                 (define (loop-without-inner eb.loop-id ... post-guard*)
                   (cond
                     [eb.pos-guard
                      (let-values ([(eb.inner-id ...) eb.inner-rhs] ...)
                        (if (and eb.pre-guard
                                 post-guard*)
                            ;; Case 2
                            (let-values ([(eb-i.outer-id ...) eb-i.outer-rhs] ...
                                         [(loop-arg*) (lambda () eb.loop-arg)] ...
                                         [(inner-id*) eb.inner-id] ... ...)
                              eb-i.outer-check
                              (loop-with-inner (loop-arg*) ... inner-id* ... ...
                                               eb-i.outer-id ... ... eb-i.loop-expr ...
                                               eb.post-guard #t))
                            (outer-is-done)))]
                     [else
                      (outer-is-done)]))
                 (define (outer-is-done)
                   ;; Case 3
                   (values id-false ... #f #f #f #f))
                 (cond
                   [inner-is-initialized?
                    (loop-with-inner loop-id* ... inner-id* ... ...
                                     i-outer-id* ... ... i-loop-id* ...
                                     post-guard* i-post-guard*)]
                   [else
                    (loop-without-inner loop-id* ... post-guard*)]))])
             ;; pre guard
             ok
             ;; post guard
             ok
             ;; loop args
             (loop-id** ...
              inner-id** ... ...
              i-loop-id** ...
              i-outer-id** ... ...
              inner-is-initialized?*
              post-guard**
              i-post-guard**))))]))

  ;; make-mark-as-variables : xs:(Listof Id)
  ;;                       -> ∃fresh(xs')
  ;;                          (Syntax[Expr[G//xs]] -> Syntax[Expr[G/xs'//]])
  ;; where xs' is {(x-symbol, x-scope U {intdef-scope}), ...}
  (define (make-mark-as-variables xs)
    ;; Contains a scope to represent the context. The scope is added to
    ;; every form within the context. A scope is represented as a unique
    ;; "token" (a value internal to the program representation).
    ;; The global environment (a table) maps a pair (symbol, scope set)
    ;; to its meaning (a variable, a syntactic form, or a transformer).
    (define intdef (syntax-local-make-definition-context))
    ;; adds to scope and the global environment
    (syntax-local-bind-syntaxes xs #f intdef)
    ;; now intdef contains
    ;; scope set {intdef-scope}
    ;; the global env contains
    ;; mapping { (x-symbol, x-scope U {intdef-scope}) => Variable, ...}
    (lambda (stx)
      (internal-definition-context-introduce intdef stx 'add))))

#|
(begin-for-syntax
  (define (mark-as-vars1) (make-mark-as-variables (syntax-list #'(x))))
  (define (mark-as-vars2) (make-mark-as-variables (syntax-list #'(x)))))
(let-syntax ([m (lambda (stx)
                  (with-syntax ([x1 (mark-as-vars1 #'x)]
                                [x2 (mark-as-vars2 #'x)])
                    #'(let ([x1 5]) x2)))])
  m)
|#

;; protect : (Listof Id) Syntax[ECR[G][G']] -> 
#;(define-for-syntax (protect binding-ids stx)
  (syntax-parse stx
    [(([(outer-id ...) outer-rhs] ...)
      outer-check
      ([loop-id loop-expr] ...)
      pos-guard
      ([(inner-id ...) inner-rhs] ...)
      pre-guard
      post-guard
      (loop-arg ...))
     ;; ==>
     #:attr o-mark-as-variables (make-mark-as-variables
                                 (subtract (syntax->list #'(outer-id ... ...)) binding-ids))
     #:attr l-mark-as-variables (make-mark-as-variables
                                 (subtract (syntax->list #'(loop-id ...)) binding-ids))
     #:attr i-mark-as-variables (make-mark-as-variables
                                 (subtract (syntax->list #'(inner-id ... ...)) binding-ids))
     ((attribute o-mark-as-variables)
      #`(([(outer-id ...) outer-rhs] ...)
         outer-check
         #,@((attribute l-mark-as-variables)
             #`(([loop-id loop-expr] ...)
                pos-guard
                #,@((attribute i-mark-as-variables)
                    #'(([(inner-id ...) inner-rhs] ...)
                       pre-guard
                       post-guard
                       (loop-arg ...)))))))]))

#;(define-sequence-syntax do/sequence2
  (lambda (stx)
    (raise-syntax-error #f "only allowed in a fast sequence context" stx))
  (lambda (stx)
    (syntax-parse stx
      [[(id:id ...) (_ (b-clause:bind-clause ...+) seq-expr:expr)] #:cut
       ;; b-clause : BindingClause[G][{b-clause.id ...}]
       ;; seq-expr : Expr[G/{b-clause.id ...}][...]       
       #:attr mark-as-variables (make-mark-as-variables
                                 (syntax->list #'(b-clause.id1 ... ...)))
       ;; mark-as-variables :
       ;; ∃(xs') (Syntax[Expr[G/{b-clause.id ...}]]
       ;;                  -> Syntax[Expr[G/xs']])
       #:with (b-clause*:bind-clause ...) (map (lambda (ids seq-expr)
                                                 #`[#,((attribute mark-as-variables) ids) #,seq-expr])
                                               (syntax->list #'((b-clause.id1 ...) ...))
                                               (syntax->list #'(b-clause.seq ...)))
       #:with eb:expanded-clause-record (merge #'(b-clause*.expanded ...))
       #:with eb-i:expanded-clause-record
              (expand-for-clause stx #`[(id ...) #,((attribute mark-as-variables) #'seq-expr)])
       #:with eb-i* (syntax-parse #'eb-i
                    [(([(outer-id ...) outer-rhs] ...)
                      outer-check
                      ([loop-id loop-expr] ...)
                      pos-guard
                      ([(inner-id ...) inner-rhs] ...)
                      pre-guard
                      post-guard
                      (loop-arg ...))
                     ;; ==>
                     #:attr o-mark-as-variables (make-mark-as-variables
                                                 (syntax->list #'(outer-id ... ...)))
                     #:attr l-mark-as-variables (make-mark-as-variables
                                                 (syntax->list #'(loop-id ...)))
                     #:attr i-mark-as-variables (make-mark-as-variables
                                                 (syntax->list #'(inner-id ... ...)))
                     ((attribute o-mark-as-variables)
                      #`(([(outer-id ...) outer-rhs] ...)
                         outer-check
                         #,@((attribute l-mark-as-variables)
                             #`(([loop-id loop-expr] ...)
                                pos-guard
                                #,@((attribute i-mark-as-variables)
                                    #'(([(inner-id ...) inner-rhs] ...)
                                       pre-guard
                                       post-guard
                                       (loop-arg ...)))))))])
       #:with ecr:expanded-clause-record (nest #'(eb eb-i*))
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

(define-sequence-syntax do/sequence2
  (lambda (stx)
    (raise-syntax-error #f "only allowed in a fast sequence context" stx))
  (lambda (stx)
    (syntax-parse stx
      [[(id:id ...) (_ (b-clause:bind-clause ...+) seq-expr:expr)] #:cut
       ;; b-clause : BindingClause[G][{b-clause.id ...}]
       ;; seq-expr : Expr[G/{b-clause.id ...}][...]       
       #:attr mark-as-variables (make-mark-as-variables
                                 (syntax->list #'(b-clause.id1 ... ...)))
       ;; mark-as-variables :
       ;; ∃(xs') (Syntax[Expr[G/{b-clause.id ...}]]
       ;;                  -> Syntax[Expr[G/xs']])
       #:with (b-clause*:bind-clause ...) (map (lambda (ids seq-expr)
                                                 #`[#,((attribute mark-as-variables) ids) #,seq-expr])
                                               (syntax->list #'((b-clause.id1 ...) ...))
                                               (syntax->list #'(b-clause.seq ...)))
       #:with eb:expanded-clause-record (merge #'(b-clause*.expanded ...))
       #:with eb-i:expanded-clause-record
              (expand-for-clause stx #`[(id ...) #,((attribute mark-as-variables) #'seq-expr)])
       #:with ecr:expanded-clause-record (nest #'((id ...) eb eb-i))
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

;;ryan: Since for/list doesn't work for multiple-valued sequences,
;;  maybe it would be better to just raise a syntax error if do/sequence
;;  is used as an expression. We could always change it later.

(define-sequence-syntax do/sequence
  (lambda (stx)
    (raise-syntax-error #f "only allowed in a fast sequence context" stx))
  (lambda (stx)
    (syntax-parse stx
      [[(id:id ...) (_ () body:expr ...+)]
       (for-clause-syntax-protect
        #'[(id ...) (:do-in ([(id ...) (begin body ...)]) #t () #t () #t #f ())])]
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
