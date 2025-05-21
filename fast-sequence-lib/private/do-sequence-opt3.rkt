#lang racket/base

(require (for-syntax racket/syntax
                     racket/base
                     racket/promise
                     syntax/parse
                     syntax/stx
                     syntax/unsafe/for-transform)
         "fast-sequence-filter.rkt")

(provide (rename-out [do/sequence do/sequence-opt3]))

(module+ private-for-testing
  (provide in-nested
           in-when
           (for-syntax bind-clause
                       when-clause
                       expanded-clause-record
                       nest
                       merge)))

;; -------------------------------------
;; How to read types
;; 
;; We use a combination of types and grammars to describe macros --- in particular, their
;; binding structure. For example,
;;
;; (let ([x 1] [y 2]) (add1 x))
;;
;; We could describe this using the following notation:
;;
;;   (let ([x 1] [y 2]) (add1 x)) : Expr[G0][Nat]
;;   where G0 represents the initial environment.
;;
;; Within that term, (add1 x) : Expr[G0/x:Nat,y:Nat][Nat].
;;
;; Expr is a nonterminal that has parameters. In the example above, it tells
;; that we can think of the term (let ([x 1] [y 2]) (add1 x)) as an expression
;; that is evaluated in type environment G0 and has type Nat. (Note that this
;; representation is not unique: it also can be described as Expr[G0][Real],
;; Expr[G0][Integer], etc. depending on what we need.)
;;
;; Also, (add1 x) in the body of the let macro is evaluated in the extended
;; environment, where x and y of type Nat are bound later than identifiers from
;; G0. We say that a type environment consists of *ribs* and G0 and [x:Nat,y:Nat]
;; are different ribs. Identifiers from newer ribs can shadow identifiers from
;; older ribs (the newest ribs are the rightmost ones in this notation), and
;; a rib cannot contain duplicate identifiers.
;;
;;
;; We can generalize from the example above and describe the binding behavior
;; of the let macro using this notation:
;;
;; (let ([id:Id expr:Expr[G][T]] ...) body:Expr[G/id:T...][T']) : Expr[G][T']
;;
;; As this example shows, macros can be described by terms of more complex structure
;; than atomic nonterminals with parameters. We say that macros are described by
;; syntax patterns. For instance, the terms ([id:Id expr:Expr[G][T]] ...),
;; Expr[G/id:T...][T'], Expr[G][T'] in this example are syntax patterns.
;; We allow syntax patterns to contain names, and nonterminals can depend on
;; those names. For example, in the specification for let, the pattern for the body is
;; Expr[G/id:T...][T'], which depends on id. So, in the example
;; (let ([x 1] [y 2]) (add1 x)), the body expression (add1 x) is described
;; by Expr[G/x:Nat,y:Nat][Nat].
;;
;; Is this notation enough for describing macros? In Racket, macros are defined
;; as functions that return syntax objects. To type-check those functions, we
;; need to connect types of Racket expressions and syntax patterns
;; describing macros.
;;
;; We describe Racket expressions using types. For example, 5 : Nat, "hello" : String.
;; Note that we overload colon to define two different relations: one relating
;; pieces of syntax and syntax patterns and another one relating Racket expressions
;; and types.
;;
;; To describe syntax objects, we use type Syntax with a parameter. The parameter is
;; a syntax pattern describing the structure of the syntax object. For example,
;;
;; (define-syntax let
;;   (syntax-parser
;;     [(_ ([var:id rhs:expr] ...) body:expr)
;;      ;; For example, #'body has type Syntax[Expr[G0/var:T1...][T]].
;;      ;; This type depends on var ..., which is a macro pattern corresponding to the
;;      ;; syntax pattern marked "id" in the describtion of the let macro above.
;;      ;;
;;      ;; This macro is correct because body and rhs are put in the right context,
;;      ;; that is, body is in the initial scope extended by var ..., and rhs is
;;      ;; in the initial scope.
;;      #'((lambda (var ...) body) rhs ...)]))
;;
;; -------------------------------------
;; BindingClause and ECR
;;
;; Two important syntax pattern that we use are BindingClause and ECR. BindingClause
;; describes a binding clause that has the same shape and meaning as a binding clause
;; of a for loop (or its variants) macro. ECR is a representation for fast sequences
;; in Racket provided by the for loop macro framework.
;;
;;   BindingClause[G][G'] -- a binding clause in type environment G
;;   that produces type environment rib G'.
;;   BindingClause[G][{x:t, ...}]
;;     ::= [(x:Id ...) Expr[G][(sequenceof (values t ...))]]
;;
;;   ECR[G][G'] -- an expanded clause record in type environment G
;;   that produces type environment rib G'.
;;   ECR[G][G']
;;   ::=
;;     (;; outer bindings
;;      ([(outer-id : Id) outer-rhs : Expr[G][O]])
;;      ;; loop bindings
;;      ([loop-id : Id loop-expr : Expr[G/outer-id : O][L]])
;;      ;; pos check
;;      pos-guard : Expr[G/outer-id : O/loop-id : L][Any]
;;      ;; inner bindings
;;      ([inner-id inner-rhs : Expr[G/outer-id : O/loop-id : L][I]] ...)
;;      ;; pre guard
;;      pre-guard : Expr[G/outer-id : O/loop-id : L/inner-id : I, ...][Any]
;;      ;; post guard
;;      post-guard : Expr[G/outer-id : O/loop-id : L/inner-id : I, ...][Any]
;;      ;; loop args
;;      (loop-arg : Expr[G/outer-id : O/loop-id : L/inner-id : I, ...][L]))
;;    where G' ⊂ { outer-id : O/loop-id : L/inner-id : I, ...}
;;               and { outer-id : O/loop-id : L/inner-id : I, ...} - dom(G')
;;               are fresh variables with respect to the for loop body.
;;
;; An expand-for-clause function provided by Racket expands a binding clause
;; that has syntax pattern BindingClause[G][G'] into an expanded clause record
;; that has syntax pattern ECR[G][G']:
;; 
;; expand-for-clause : Syntax BindingClause[G][G'] -> ECR[G][G']
;;
;; -------------------------------------
;;
;; Decomposition strategy for do/sequence:
;; - in-nested -- handles nested iteration
;; - in-when   -- handles when-clauses (generalizes filter)
;; - in-body   -- handles body (generalizes map)
;; 
;; Example:
;; 
;; (do/sequence ([x XS]
;;               #:when COND1
;;               [y YS]
;;               [w WS]
;;               #:when COND2
;;               [z ZS])
;;   BODY)
;; where each when-clause's and the following chunk of binding clause's expressions are
;; in the scope of all variables bound by binding clauses preceding them
;; =
;; (in-nested ([x XS])
;;  (in-nested ([() (in-when COND1)])     ; from #:when COND1
;;    (in-nested ([y YS] [w WS])
;;      (in-nested ([() (in-when COND2)]) ; from #:when COND2
;;        (in-nested ([z ZS])
;;          (in-body BODY))))))
;;
;; A sequence is represented by the type ECR (Expanded Clause Record) that
;; contains information about iteration.
;;
;; The implementation of in-nested is decomposed to two functnions:
;; - merge -- merges ECRs of multiple binding clauses into a single ECR.
;; - nest  -- nests two ECRs. The result is a single ECR.
;;
;; -------------------------------------

;; A helper sequence that contains/represents information
;; about a number of iterations: 1 or 0.
;;
;;   (in-when #t) = [(values)]
;;   (in-when #f) = []
(define-sequence-syntax in-when
  (lambda (stx)
    (raise-syntax-error #f "only allowed in a fast sequence context" stx))
  (lambda (stx)
    (syntax-case stx ()
      [[(id ...) (_ expr)]
       (for-clause-syntax-protect
        (with-syntax ([(expr*) (generate-temporaries #'(expr))])
          #'[(id ...) (:do-in ([(expr*) (lambda () expr)]) #t () #t () (expr*) #f ())]))]
      [_ #f])))

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
              (loop-arg ...)]))
  
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

  ;; A when-clause.
  ;;
  ;; when-clause =
  ;;   | #:when when-guard
  (define-splicing-syntax-class when-clause
    (pattern (~seq #:when condition:expr)
                   #:with bc #'[() (in-when condition)]))

  ;; A chunk of do/sequence clauses.
  (define-splicing-syntax-class chunk #:attributes ([b 1]) ;; each b is a BindingClause
    (pattern (~seq b:bind-clause ...+ ~!))
    (pattern (~seq w:when-clause ...+ ~!)
             #:with (b ...) #'(w.bc ...))))

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

  ;; merge : Syntax[(ECR[G][{x:t ...}] ...)] -> Syntax[ECR[G][{x:t ... ...}]]
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
          (ecr.loop-arg ... ...))])))

(begin-for-syntax
  ;; nest : Syntax[((id:Id ...) ECR[G][{x ...}] Expr[G/x...][Any] ECR[G/x...][{id ...}])]
  ;;     -> Syntax[ECR[G][{id ...}]]
  (define (nest stx)
    (syntax-parse stx
      [((id:id ...)
        eb:expanded-clause-record
        when-cond:expr
        eb-i:expanded-clause-record)
       (with-syntax ([(post-guard* i-post-guard* inner-is-initialized? ok)
                      (generate-temporaries #'(post-guard* i-post-guard* inner-is-initialized? ok))]
                     [(post-guard** i-post-guard** inner-is-initialized?*)
                      (generate-temporaries #'(post-guard** i-post-guard** inner-is-initialized?*))]
                     [(id-false ...) (build-list
                                       (length (syntax->list #'(eb-i.loop-id ... eb.loop-id ... eb.inner-id ... ...
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
                            (if when-cond
                                (let-values ([(eb-i.outer-id ...) eb-i.outer-rhs] ...
                                             [(loop-arg*) (lambda () eb.loop-arg)] ...
                                             [(inner-id*) eb.inner-id] ... ...)
                                  eb-i.outer-check
                                  (loop-with-inner (loop-arg*) ... inner-id* ... ...
                                                   eb-i.outer-id ... ... eb-i.loop-expr ...
                                                   eb.post-guard #t))
                                (loop-without-inner eb.loop-arg ... eb.post-guard))
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
      (internal-definition-context-introduce intdef stx 'add)))

  ;; An in-body expression.
  ;; in-body-expr =
  ;;   | (in-body body ...+)
  (define-syntax-class in-body-expr
    #:literals (in-body)
    (pattern (in-body body:expr ...+))))

;; in-nested-optimize-body : Syntax[BindingClause[G][{id:t ...}]] -> Syntax[ECR[G][{id:t ...}]]
;; An optimization for the special case:
;;   (in-nested ([x XS] ...) (in-body BODY))
(define-for-syntax (in-nested-optimize-body stx)
  (syntax-parse stx
    #:literals (in-nested)
    [[(id:id ...) (in-nested (b-clause:bind-clause ...+) ib:in-body-expr)]
     #:attr mark-as-variables (make-mark-as-variables
                               (syntax->list #'(b-clause.id1 ... ...)))
     #:with (b-clause*:bind-clause ...) (map (lambda (ids seq-expr)
                                               #`[#,((attribute mark-as-variables) ids) #,seq-expr])
                                             (syntax->list #'((b-clause.id1 ...) ...))
                                             (syntax->list #'(b-clause.seq ...)))
     #:with ecr:expanded-clause-record (merge #'(b-clause*.expanded ...))
     #:with ib*:in-body-expr ((attribute mark-as-variables) #'ib)
     (with-syntax ([(ok) (generate-temporaries #'(ok))]
                   [(id* ...) (generate-temporaries #'(id ...))]
                   [(false* ...) (build-list
                                  (length (syntax->list #'(ecr.inner-id ... ... id ...)))
                                  (lambda (x) #'#f))])
       #'[(id ...)
          (:do-in
           ([(ecr.outer-id ...) ecr.outer-rhs] ...)
           ecr.outer-check
           ([ecr.loop-id ecr.loop-expr] ...)
           ecr.pos-guard
           ([(ecr.inner-id ... ... id ... ok)
             (let-values ([(ecr.inner-id ...) ecr.inner-rhs] ...)
               (cond
                 [ecr.pre-guard
                  (let-values ([(id* ...) (begin ib*.body ...)])
                    (values ecr.inner-id ... ... id* ... #t))]
                 [else
                  (values false* ... #f)]))])
           ok
           ecr.post-guard
           (ecr.loop-arg ...))])]))

;; in-nested* : Syntax[BindingClause[G][{id:t ...}]] -> Syntax[ECR[G][{id:t ...}]]
(define-for-syntax (in-nested* stx cond-stx)
  (syntax-parse stx
    #:literals (in-nested)
    [[(id:id ...) (in-nested (b-clause:bind-clause ...+) seq-expr:expr)]
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
     #:with ecr:expanded-clause-record
            (nest #`((id ...) eb #,((attribute mark-as-variables) cond-stx) eb-i))
     #'[(id ...)
        (:do-in
         ([(ecr.outer-id ...) ecr.outer-rhs] ...)
         ecr.outer-check
         ([ecr.loop-id ecr.loop-expr] ...)
         ecr.pos-guard
         ([(ecr.inner-id ...) ecr.inner-rhs] ...)
         ecr.pre-guard
         ecr.post-guard
         (ecr.loop-arg ...))]]))

;; in-nested : Syntax[BindingClause[G][{id:t ...}]] -> Syntax[ECR[G][{id:t ...}]]
(define-sequence-syntax in-nested
  (lambda (stx)
    (raise-syntax-error #f "only allowed in a fast sequence context" stx))
  (lambda (stx)
    (syntax-parse stx
      #:literals (in-nested in-when)
      ;; Special case: (in-nested ([x XS] ...) (in-body BODY))
      [[(id:id ...) (_ (b-clause:bind-clause ...+) ib:in-body-expr)]
       (in-nested-optimize-body stx)]
      ;; Special case: (in-nested ([x XS] ...) (in-nested ([() (in-when COND)] ...) YS))
      #;[[(id:id ...) (_ (b-clause:bind-clause ...+)
                       (in-nested ([() (in-when cond:expr)] ...+) seq-expr:expr))]
       #'[(id ...)
          (in-nested ([(b-clause.id1 ... ...) (fast-sequence-filter
                                               (lambda (b-clause.id1 ... ...) (and cond ...))
                                               (do/sequence (b-clause ...) (values b-clause.id1 ... ...)))])
                     seq-expr)]]
      ;; Special case (v.2): (in-nested ([x XS] ...) (in-nested ([() (in-when COND)] ...) YS))
      [[(id:id ...) (_ (b-clause:bind-clause ...+)
                       (in-nested ([() (in-when cond:expr)] ...+) seq-expr:expr))]
       (in-nested* #'[(id ...) (in-nested (b-clause ...) seq-expr)] #'(and cond ...))]
      ;; Special case: (in-nested ([() (in-when COND)] ...) YS)
      #;[[(id:id ...) (_ ([() (in-when cond:expr)] ...+) seq-expr:expr)]
       #:with ecr:expanded-clause-record (expand-for-clause stx #'[(id ...) seq-expr])
       (with-syntax ([(false* ...) (build-list
                                    (length (syntax->list #'(ecr.outer-id ... ...)))
                                    (lambda (x) #'#f))]
                     [(cond-val) (generate-temporaries #'(cond-val))])
         #'[(id ...)
            (:do-in
             ([(ecr.outer-id ... ... cond-val)
               (let ([cond-val (and cond ...)])
                 (if cond-val
                     (let-values ([(ecr.outer-id ...) ecr.outer-rhs] ...)
                       (values ecr.outer-id ... ... #t))
                     (values false* ... #f)))])
             (and cond-val ecr.outer-check)
             ([ecr.loop-id (and cond-val ecr.loop-expr)] ...)
             (and cond-val ecr.pos-guard)
             ([(ecr.inner-id ...) ecr.inner-rhs] ...)
             ecr.pre-guard
             ecr.post-guard
             (ecr.loop-arg ...))])]
      ;; General case: (in-nested ([x XS] ...) YS)
      [[(id:id ...) (_ (b-clause:bind-clause ...+) seq-expr:expr)]
       (in-nested* stx #'#t)]
      [_ (raise-syntax-error #f "got something else" stx)])))

;; A helper sequence that handles do/sequence's body.
;; (in-body body ...) = [(begin body ...)]
(define-sequence-syntax in-body
  (lambda (stx)
    (raise-syntax-error #f "only allowed in a fast sequence context" stx))
  (lambda (stx)
    (syntax-parse stx
      [[(id:id ...) (_ body:expr ...+)]
       (for-clause-syntax-protect
        #'[(id ...) (:do-in ([(id ...) (begin body ...)]) #t () #t () #t #f ())])]
      [_ #f])))

;; optimize-when : Syntax[(in-nested (BindingClause[G][{x:t ...}] ...) Expr[G/x:t...][(sequenceof (values t ...))])]
;;              -> Syntax[(in-nested (BindingClause[G][{x:t ...}] ...) Expr[G/x:t...][(sequenceof (values t ...))])]
(define-for-syntax (optimize-when stx)
  (define-syntax-class true-literal
    (pattern (~and #t t) #:when (free-identifier=? #'#%datum (datum->syntax #'t '#%datum))))
  (syntax-parse stx
    #:literals (in-nested in-when)
    ;; An optimization for the special case:
    ;;   (in-nested ([() (in-when #t)] ...) YS)
    [(in-nested ([() (in-when _:true-literal)] ...+) seq-expr:expr)
     (optimize-when #'seq-expr)]
    ;; An optimization for the special case:
    ;;   (in-nested ([x XS] ...) (in-nested ([() (in-when #t)] ...) (in-body BODY)))
    [(in-nested (b-clause:bind-clause ...+) (in-nested ([() (in-when _:true-literal)] ...+) ib:in-body-expr))
     #'(in-nested (b-clause ...) ib)]
    [(in-nested (b-clause:bind-clause ...+) seq-expr:expr)
     #`(in-nested (b-clause ...) #,(optimize-when #'seq-expr))]
    [_ stx]))

;; TODO:
;; - type annotations and an explanation of types

;; A fast sequence form that allows iterating over nested sequences.
;; Example:
;; (do/sequence ([(x) (in-vector vec-of-lsts)]
;;               #:when #t
;;               [(y) (in-list x)])
;;   y)
;; Clauses of do/sequence have the same meaning as a for loop clauses.
;; Using do/sequence outside a for loop is forbidden.
(define-sequence-syntax do/sequence
  (lambda (stx)
    (raise-syntax-error #f "only allowed in a fast sequence context" stx))
  (lambda (stx)
    (syntax-parse stx
      #:context 'do/sequence
      [[(id:id ...) (_ (c:chunk ...) body:expr ...+)]
       (for-clause-syntax-protect
        #`[(id ...) #,(optimize-when (foldr (lambda (chunk acc) #`(in-nested #,chunk #,acc))
                                            #'(in-body body ...)
                                            (syntax->list #'((c.b ...) ...))))])])))
