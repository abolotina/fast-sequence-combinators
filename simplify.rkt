#lang racket/base
(require racket/match
         racket/pretty
         compiler/decompile
         compiler/zo-parse)

(define known-procedures
  '(values unsafe-car unsafe-cdr #%apply-values reverse cons))

(define (known-procedure? x)
  (and (symbol? x) (memq x known-procedures)))

(define (simplify e)
  (match e
    [(list* 'module name lang body)
     (list* 'module name lang (map simplify body))]
    [(list 'define-values (list name) rhs)
     (list 'define name (simplify rhs))]
    [(list* 'lambda args (list 'quote info) ... (? pair? body))
     ;; like the syntax pattern (lambda args (quote info) ... body ...+)
     (list* 'lambda args (map simplify body))]
    [(list 'if e1 e2 e3)
     (list 'if (simplify e1) (simplify e2) (simplify e3))]
    [(list* 'let bindings body)
     (list* 'let (map simplify-binding bindings) (map simplify body))]
    [(list* 'begin exprs)
     (simplify-begin (map simplify exprs))]
    [(list '#%sfs-clear var)
     var]
    [(list* (? known-procedure? f) args)
     (list* f (map simplify args))]
    [(list '#%closed name lam-expr)
     (simplify lam-expr)]
    [(list* (list '#%closed name lam-expr) args)
     (list* (simplify-closure lam-expr name) (map simplify args))]
    [(list 'set!-values ids expr)
     (list 'set!-values ids (simplify expr))]
    [(list '#%set!-rec-values ids expr)
     (list '#%set!-rec-values ids (simplify expr))]
    [e e]))

(define (simplify-binding b)
  (match b
    [(list vars rhs) (list vars (simplify rhs))]))

(define (simplify-closure lam name)
  (match (simplify lam)
    [(list* 'lambda args body)
     (list* 'named-lambda name args body)]))

(define (simplify-begin body)
  (define (useless? e)
    (match e
      [(list 'quote _) #t]
      [(? symbol?) #t]
      [_ #f]))
  (match body
    [(list* (? useless?) ... (? pair? body))
     (match body
       [(list e) e]
       [body (cons 'begin body)])]))


(match (current-command-line-arguments)
  [(vector filename)
   (parameterize ((pretty-print-columns 200))
     (pretty-write
      (simplify
       (decompile
        (zo-parse (open-input-file filename))))))])
