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

;; The do/sequence implementation consists of a base case and two recursive cases.
;;
;; Base case:
;;
;;   (do/sequence () body) = [body]
;;
;; Recursive cases:
;;
;; The recursive cases are defined via an intermediate macro, do/sequence2,
;; that implements a pairwise case.
;;
;;   (do/sequence2 bind-chunk seq-expr)
;;
;; Example:
;;
;;   (for/list ([(a) (do/sequence2 ([(x) (list 10 20 30)]) (list x (add1 x)))]) a)
;;   = (list 10 11 20 21 30 31)
;;
;; Decomposing the entire solution into two sequence transformers
;; allows recurring in the most natural way:
;;
;;   (do/sequence (bind-chunk . rest) body)
;;   = (do/sequence2 bind-chunk (do/sequence rest body))
;;
;; Then, since the when-chunk case only affects a number of iterations,
;; we can turn it into a bind-chunk that encodes that information:
;;
;;   (do/sequence (when-chunk . rest) body)
;;   = (do/sequence2 ([() (in-nullary-relation when-chunk-as-expr)])
;;                 (do/sequence rest body))
;;
;; ===================================
;; Concrete example
;;
;; A small concrete example of do/sequence usage and its handwritten "expansion"
;; below shows the essential structure of the solution for the pairwise case.
;;
;; (for ([y (do/sequence ([lst '((1 2) () (2 4) (5 6))]
;;                        #:when #t
;;                        [x (in-list lst)]
;;                        #:when (odd? x))
;;            (list lst x))])
;;   (println y)) ; should print 1, 5
;;
;; ==>
;;
;; Goal: It finds the next element for x or decides that x is done.
;; Must bind x, x-is-found.

;;ryan: Why must bind x-is-found? In what expression(s)? I think what
;;  you mean is the *goal* is to bind x, and part of the *implementation
;;  strategy* is to use an auxiliary variable x-is-found to indicate
;;  whether the sequence has a next value.

;; The solution essentially has the structure of the following recursive function.
;; It keeps two variables:
;; - outer-seq is the part of the outer sequence that we haven't looked into yet.
;; - inner-seq is the remainder of the inner sequence that we haven't processed yet.
;;
;; Then, the algorithm is the following:
;; 
;; - It looks into the inner sequence and gets the next value for x or,
;; - If the inner sequence is empty, it looks into the outer sequence and gets the
;;   next inner sequence or,
;; - If the outer sequence is empty then x is done.

;;ryan: Good!

;; (let loop ([outer-seq '((1 2) () (2 4) (5 6) (2 7))]
;;            [inner-seq '()])
;;   (when #t
;;     (let-values
;;         ([(x outer-seq* inner-rest x-is-found)
;;           (let loop* ([outer-seq* outer-seq]
;;                       [inner-seq* inner-seq])
;;             (cond [(pair? inner-seq*)
;;                    (let ([x (car inner-seq*)]
;;                          [inner-rest (cdr inner-seq*)])
;;                      (cond [(odd? x)
;;                             (values x outer-seq* inner-rest #t)]
;;                            [else
;;                             (loop* outer-seq* inner-rest)]))]
;;                   [else
;;                    (cond [(pair? outer-seq*)
;;                           (let ([inner-lst (car outer-seq*)]
;;                                 [outer-rest (cdr outer-seq*)])
;;                             (cond [#t
;;                                    (loop* outer-rest inner-lst)]
;;                                   [else
;;                                    (loop* outer-rest inner-seq*)]))]
;;                   [else
;;                    (values #f #f #f #f)])]))])
;;       (when x-is-found
;;         (println x)
;;         (loop outer-seq* inner-rest)))))

;;ryan: The indentation is incorrect for the final else clause.

;;ryan: Flatten cond into 3 cases, following the 3 cases in your
;;  algorithm description.

;;ryan: Why are the #t expressions there? (After when at the top, in a
;;  cond clause near the bottom.) If they are placeholders for
;;  something else, maybe add a comment. Or maybe delete them, if you
;;  don't want to talk about them.

;;ryan: Here is the code with indentation fixed and cond clauses flattened:
#|
(let loop ([outer-seq '((1 2) () (2 4) (5 6) (2 7))]
           [inner-seq '()])
  (when #t
    (let-values
        ([(x outer-seq* inner-rest x-is-found)
          (let loop* ([outer-seq* outer-seq]
                      [inner-seq* inner-seq])
            (cond [(pair? inner-seq*)
                   (let ([x (car inner-seq*)]
                         [inner-rest (cdr inner-seq*)])
                     (cond [(odd? x)
                            (values x outer-seq* inner-rest #t)]
                           [else
                            (loop* outer-seq* inner-rest)]))]
                  [(pair? outer-seq*)
                   (let ([inner-lst (car outer-seq*)]
                         [outer-rest (cdr outer-seq*)])
                     (cond [#t
                            (loop* outer-rest inner-lst)]
                           [else
                            (loop* outer-rest inner-seq*)]))]
                  [else
                   (values #f #f #f #f)]))])
      (when x-is-found
        (println x)
        (loop outer-seq* inner-rest)))))
|#

;;
;; ===================================
;; Generalization
;;
;; We can generalize a concrete example from above for arbitrary sequences
;; by using expanded clause records.
;;
;; For simplicity, suppose we have two nested sequences (for two chunks of sequences,
;; we will need to just add more ellipses). The expanded clause records contain
;; components returned by expand-for-clause.
;; In the following,
;; - the components for the outer sequence are prefixed with o-
;; - the components for the inner sequence are prefixed with i-

;;ryan: good, helpful convention

;;ryan: It would help if you listed the contents of an expanded clause
;;  record here, especially if you omit some components for simplicity
;;  (and you should). It's also helpful to connect the components with
;;  the previous concrete example.

;;ryan: Before you continue reading, think for a moment about this question:
;;
;;  - What are the differences between the concrete example and the
;;    true, general implementation?
;;
;;  Now, think about these questions:
;;
;;  - Can you break those differences down into multiple, smaller differences?
;;
;;  - If so, which differences are the most important for you to
;;    explain carefully, and which ones could you say "It's an obvious
;;    generalization."?
;;
;;  (I have a simplification in mind, and I can't think of better
;;  questions to introduce it. So think about the questions for a bit,
;;  then read on, and then come back and think about the questions
;;  again and see if you understand why I'm proposing the change.)



;; ...



;; ...



;; ...



;;ryan: You could further simplify this generalized version by
;;  saying "let's assume that each sequence had a single loop-id, a
;;  single pos-guard expression, and so on". It would eliminate the
;;  distracting ellipses, and the generalization to multiple
;;  identifiers and multiple expressions is straightforward. The more
;;  important difference to explain at this point is how the arbitrary
;;  identifiers and expressions produced by the sequences interact ---
;;  mainly, how to get the scoping correct. It would also help clarify
;;  which identifiers come from the records and which names you
;;  generated using generate-temporaries.


;; As a first approximation of the solution, we replace the pieces
;; of information in the loop skeleton that come from two sequences
;; in the concrete example with components from their expanded clause records.
;;
;; (let-values ([(o-outer-id) o-outer-expr])
;;   (let loop ([o-loop-id o-loop-expr]
;;              [i-loop-id uninitialized]
;;              [inner-is-initialized? #f])
;;     (when #t
;;       (let-values
;;           ([(o-loop-id* i-loop-id** id ... inner-is-initialized? next-is-found)
;;             (let loop* ([o-loop-id* o-loop-id]
;;                         [i-loop-id* i-loop-id]
;;                         [inner-is-initialized? inner-is-initialized?])
;;               (let ([i-pos-guard* (lambda (i-loop-id) i-pos-guard)]
;;                     [i-inner-rhs* (lambda (i-loop-id) i-inner-rhs)] ...
;;                     [i-loop-arg* (lambda (i-loop-id) i-loop-arg)]
;;                     [o-pos-guard* (lambda (o-loop-id) o-pos-guard)]
;;                     [o-inner-rhs* (lambda (o-loop-id) o-inner-rhs)] ...
;;                     [o-loop-arg* (lambda (o-loop-id) o-loop-arg)])
;;                 (cond [(and inner-is-initialized? (i-pos-guard* i-loop-id*))
;;                        (let ([i-inner-id (i-inner-rhs* i-loop-id*)] ...)
;;                          (cond [i-when-guard
;;                                 (values o-loop-id* (i-loop-arg* i-loop-id*) id ... #t #t)]
;;                                [else
;;                                 (loop* o-loop-id* (i-loop-arg* i-loop-id*) #t)]))]
;;                       [(o-pos-guard* o-loop-id*)
;;                         (let ([o-inner-id (o-inner-rhs* o-loop-id*)] ...)
;;                           (cond [o-when-guard
;;                                  (loop* (o-loop-arg* o-loop-id*)
;;                                         (let-values ([(i-outer-id) i-outer-expr])
;;                                           i-loop-expr) #t)]
;;                                 [else
;;                                  (loop* (o-loop-arg* o-loop-id*)
;;                                         i-loop-id* #f)]))]
;;                       [else
;;                        (values #f #f #f ... #f #f)])))])
;;         (when next-is-found
;;           do-body
;;           (loop o-loop-id* i-loop-id** inner-is-initialized?))))))
;;
;; This version will work for most of fast sequence forms, incuding the sequences
;; from the concrete example. But the actual implementation takes into account
;; more complex scoping rules. In particular, in this current form, it fails for
;; generic sequences, that is, sequences that are not fast sequence forms, like
;; (list 1 2 3) or (vector 'a 'b 'c). Their expanded clause records all have the
;; same form; for example, consider the following result of expanding the clause
;; [(x) (list 1 2 3)]:
;;
;; (([(pos->vals pos-pre-inc pos-next init pos-cont? val-cont? all-cont?)
;;    (make-sequence '(x) (list 1 2 3))])
;;  (void)
;;  ([pos init])
;;  (if pos-cont? (pos-cont? pos) #t)
;;  ([(x all-cont?/pos)
;;    (let-values ([(x) (pos->vals pos)])
;;      (values x (and all-cont? (lambda (pos) (all-cont? pos x)))))]
;;   [(pos) (if pos-pre-inc (pos-pre-inc pos) pos)])
;;  (if val-cont? (val-cont? x) #t)
;;  (if all-cont?/pos (all-cont?/pos pos) #t)
;;  ((pos-next pos)))
;;
;; It binds outer-ids for several functions, and then uses them in the further components.
;; So if the inner loop iterates only on loop-ids, then a problem appears when it looks
;; at the inner sequence first time when it hasn't looked at the outer sequence yet.
;; In the generalized version above, an attempt to check the pos-guard for the inner
;; sequence causes scoping violation because pos-cont? is not bound. To fix this issue,
;; we make the inner loop also iterate on outer-ids of inner sequences and on an
;; additional variable, ids-ok, that says whether all the necessary ids are already
;; bound before looking at the inner sequence.
;;
;; ===================================
;; A few more details
;;
;; There are a few small tricky parts not covered so far.

;; ...



;; - If the inner sequence is not empty, look into the inner sequence and find the next value for x.
;;   Check pre- and post-guard for it, and if it is true, return it or,
;; - Else look into the outer sequence and find the
;;   next inner sequence. Check pre- and post-guard for it, and if it is true,
;;   return it or,
;; - Else then x is done.

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
  
  ;; merge : PECR[G][G'][{outer-id:a, ...}][{loop-id:c, ...}][{inner-id:e, ...}] ... -> ECR[G][G'...][Go'][Gl'][Gi']
  (define (merge stx)
    (syntax-parse stx
      [(ecr:expanded-clause-record ...)
       #:with (pecr:expanded-clause-record ...) #'(ecr.protected ...)
       #'(;; outer bindings
          ;; pecr.outer-rhs : Expr[G][a] ... ...
          ([(pecr.outer-id ...) pecr.outer-rhs] ... ...
           ;; ...
           ;; extra stuff
           ;; ...
           )
          ;; outer check
          ;; pecr.outer-check : Expr[(List* Rib[{outer-id : a}] G)][Any] ...
          (and pecr.outer-check ...)
          ;; loop bindings
          ;; pecr.loop-expr : Expr[(List* Rib[{outer-id : a}] G)][c] ... ...
          ([pecr.loop-id pecr.loop-expr] ... ...)
          ;; pos check
          ;; pecr.pos-guard : Expr[(List* Rib[{loop-id : c}] Rib[{outer-id : a}] G)][Any] ...
          (and pecr.pos-guard ...)
          ;; inner bindings
          ;; pecr.inner-rhs : Expr[(List* Rib[{loop-id : c}] Rib[{outer-id : a}] G)][e] ... ...
          ([(pecr.inner-id ...) pecr.inner-rhs] ... ...)
          ;; pre guard
          ;; pecr.pre-guard : Expr[(List* Rib[{inner-id : e, ...}] Rib[{loop-id : c}] Rib[{outer-id : a}] G)][Any] ...
          (and pecr.pre-guard ...)
          ;; post guard
          ;; pecr.post-guard : Expr[(List* Rib[{inner-id : e, ...}] Rib[{loop-id : c}] Rib[{outer-id : a}] G)][Any] ...
          (and pecr.post-guard ...)
          ;; loop args
          ;; pecr.loop-arg : Expr[(List* Rib[{inner-id : e, ...}] Rib[{loop-id : c}] Rib[{outer-id : a}] G)][c] ... ...
          (pecr.loop-arg ... ...))]))

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

(begin-for-syntax
  ;; nest : Syntax[(ExpandedClauseRecord ExpandedClauseRecord)] -> Syntax[ExpandedClauseRecord]
  (define (nest stx)
    (syntax-parse stx
      [(eb:expanded-clause-record
        eb-i:expanded-clause-record)
       (with-syntax* ([(loop-id* ...) (generate-temporaries #'(eb.loop-id ...))]
                      [(loop-id** ...) (generate-temporaries #'(eb.loop-id ...))]
                      [(loop-id*** ...) (generate-temporaries #'(eb.loop-id ...))]
                      [(loop-arg-id ...) (generate-temporaries #'(eb.loop-id ...))]
                      [(loop-arg* ...) #'((loop-arg-id eb.loop-id ... eb.inner-id ... ...) ...)]
                      [(loop-arg** ...) (syntax->list #'(eb.loop-arg ...))]
                      [(false* ...) (build-list
                                     (length (syntax->list #'(eb-i.loop-id ... eb.loop-id ...)))
                                     (lambda (x) #'#f))]
                      [(id-false ...) (build-list
                                       (length (syntax->list #'(eb-i.outer-id ... ... eb-i.loop-id ... eb-i.inner-id ... ...)))
                                       (lambda (x) #'#f))]
                      [(i-outer-id-id1-false ...) (build-list
                                                   (length (syntax->list #'(eb.inner-id ... ... eb-i.outer-id ... ...)))
                                                   (lambda (x) #'#f))]
                      [(i-outer-check-id pos-guard-id i-pos-guard-id i-pos-guard* ids-ok ids-ok*
                                         process-outer-seqs)
                       (generate-temporaries #'(i-outer-check-id pos-guard-id i-pos-guard-id i-pos-guard* ids-ok ids-ok*
                                                process-outer-seqs))]
                      [pos-guard-id* #'(pos-guard-id eb.outer-id ... ... eb.loop-id ...)]
                      [(empty ...) (build-list
                                    (length (syntax->list #'(eb-i.loop-id ...)))
                                    (lambda (x) #''()))]
                      [(i-loop-id* ...) (generate-temporaries #'(eb-i.loop-id ...))]
                      [(i-loop-id** ...) (generate-temporaries #'(eb-i.loop-id ...))]
                      [(i-loop-id*** ...) (generate-temporaries #'(eb-i.loop-id ...))]
                      [(i-loop-id**** ...) (generate-temporaries #'(eb-i.loop-id ...))]
                      [(i-loop-arg-id ...) (generate-temporaries #'(eb-i.loop-arg ...))]
                      [(i-loop-arg* ...)
                       #'((i-loop-arg-id eb-i.outer-id ... ... eb-i.loop-id ... eb-i.inner-id ... ...) ...)]
                      [(i-loop-arg** ...) (syntax->list #'(eb-i.loop-arg ...))]
                      [i-outer-check* #'(i-outer-check-id eb-i.outer-id ... ...)]
                      [(loop*) (generate-temporaries #'(loop*))]
                      [(body*) (generate-temporaries #'(body*))]
                      [((outer-id*** ...) ...) (gen-temp-tree 2 #'((eb.outer-id ...) ...))]
                      [(i-outer-id* ...) (generate-temporaries #'(eb-i.outer-id ... ...))]
                      [(i-outer-id** ...) (generate-temporaries #'(eb-i.outer-id ... ...))]
                      [(i-outer-id*** ...) (generate-temporaries #'(eb-i.outer-id ... ...))]
                      [((i-outer-id**** ...) ...) (gen-temp-tree 2 #'((eb-i.outer-id ...) ...))]
                      [(inner-id* ...) (generate-temporaries #'(eb.inner-id ... ...))]
                      [(inner-id** ...) (generate-temporaries #'(eb.inner-id ... ...))]
                      [((inner-id*** ...) ...) (gen-temp-tree 2 #'((eb.inner-id ...) ...))]
                      [((i-inner-id**** ...) ...) (gen-temp-tree 2 #'((eb-i.inner-id ...) ...))]
                      [(post-guard* i-post-guard*) (generate-temporaries #'(post-guard* i-post-guard*))]
                      [(post-guard** i-post-guard**) (generate-temporaries #'(post-guard** i-post-guard**))])
         (for-clause-syntax-protect
          #'(;; outer bindings
             ([(eb.outer-id ...) eb.outer-rhs] ...
              [(pos-guard-id) (lambda (outer-id*** ... ... loop-id*** ...)
                                (let ([eb.outer-id outer-id***] ... ...)
                                  (let ([eb.loop-id loop-id***] ...)
                                    eb.pos-guard)))]
              [(i-outer-check-id) (lambda (eb-i.outer-id ... ...) eb-i.outer-check)]
              [(i-loop-arg-id) (lambda (i-outer-id**** ... ... i-loop-id**** ... i-inner-id**** ... ...)
                                 (let ([eb-i.outer-id i-outer-id****] ... ...)
                                   (let ([eb-i.loop-id i-loop-id****] ...)
                                     (let ([eb-i.inner-id i-inner-id****] ... ...)
                                       i-loop-arg**))))] ...)
             ;; outer check
             eb.outer-check
             ;; loop bindings
             ([loop-id* eb.loop-expr] ...
              [i-loop-id*** #f] ...
              [inner-id* #f] ...
              [i-outer-id* #f] ...
              [ids-ok #f]
              [post-guard* #t]
              [i-post-guard* #t])
             ;; pos check
             #t
             ;; inner bindings
             ([(i-loop-id** ... loop-id** ... ok eb-i.outer-id ... ... eb-i.loop-id ... eb-i.inner-id ... ...
                inner-id** ... i-outer-id** ... ids-ok* post-guard** i-post-guard**)
               (let ([eb.loop-id loop-id*] ...
                     [loop-arg-id (lambda (loop-id*** ... inner-id*** ... ...)
                                    (let ([eb.loop-id loop-id***] ...)
                                      (let ([eb.inner-id inner-id***] ... ...)
                                        loop-arg**)))] ...)
                 (define (loop-with-inner loop-id*** ...
                                          post-guard* i-post-guard*
                                          inner-id*** ... ... i-outer-id**** ... ... i-loop-id**** ...)
                   (let ([eb.loop-id loop-id***] ...)
                     (let ([eb.inner-id inner-id***] ... ...)
                       (let ([eb-i.outer-id i-outer-id****] ... ...)
                         (let ([eb-i.loop-id i-loop-id****] ...)
                           (cond
                             [eb-i.pos-guard
                              (let-values ([(eb-i.inner-id ...) eb-i.inner-rhs] ...)
                                (if (and eb-i.pre-guard
                                         i-post-guard*)
                                    ;; Case 1
                                    (values i-loop-arg* ... eb.loop-id ... #t
                                            eb-i.outer-id ... ... eb-i.loop-id ... eb-i.inner-id ... ...
                                            eb.inner-id ... ... eb-i.outer-id ... ... #t
                                            post-guard* eb-i.post-guard)
                                    (loop-without-inner eb.loop-id ... post-guard*)))]
                             [else
                              (loop-without-inner eb.loop-id ... post-guard*)]))))))
                 (define (loop-without-inner eb.loop-id ... post-guard*)
                   (cond
                     [pos-guard-id*
                      (let-values ([(eb.inner-id ...) eb.inner-rhs] ...)
                        (if (and eb.pre-guard
                                 post-guard*)
                            ;; Case 2
                            (let-values ([(eb-i.outer-id ...) eb-i.outer-rhs] ...)
                              i-outer-check*
                              (loop-with-inner loop-arg* ...
                                               eb.post-guard #t
                                               eb.inner-id ... ...
                                               eb-i.outer-id ... ...
                                               eb-i.loop-expr ...))
                            (outer-is-done)))]
                     [else
                      (outer-is-done)]))
                 (define (outer-is-done)
                   ;; Case 3
                   (values false* ... #f id-false ... i-outer-id-id1-false ... #f #f #f))
                 (cond
                   [ids-ok
                    (loop-with-inner eb.loop-id ...
                                     post-guard* i-post-guard*
                                     inner-id* ...
                                     i-outer-id* ...
                                     i-loop-id*** ...)]
                   [else
                    (loop-without-inner eb.loop-id ... post-guard*)]))])
             ;; pre guard
             ok
             ;; post guard
             ok
             ;; loop args
             (loop-id** ... i-loop-id** ... inner-id** ... i-outer-id** ... ids-ok* post-guard** i-post-guard**))))])))

(define-sequence-syntax do/sequence2
  (lambda (stx)
    (raise-syntax-error #f "only allowed in a fast sequence context" stx))
  (lambda (stx)
    (syntax-parse stx
      [[(id:id ...) (_ (b-clause:bind-clause ...+) seq-expr:expr)] #:cut
       #:with eb:expanded-clause-record (merge #'(b-clause.expanded ...))
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
