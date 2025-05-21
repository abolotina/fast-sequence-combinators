#lang racket

(require (for-syntax syntax/parse))

(module+ private
  (provide (for-syntax expanded-clause-record)))

(begin-for-syntax
  ;; An expanded clause record. Contains information to fill in the loop
  ;; skeleton: how to start looping, how to decide whether to stop,
  ;; how to bind loop variables, how to recur, and a few more checks.
  (define-syntax-class expanded-clause-record
    (pattern [([(outer-id:id ...) outer-rhs:expr #|Expr[G][(values tO ...)]|#] ...)
              outer-check:expr #|Expr[G/∆O][Boolean]|#
              ([loop-id:id loop-expr:expr #|Expr[G/∆O][tL]|#] ...)
              pos-guard:expr #|Expr[G/∆O/∆L][Boolean]|#
              ([(inner-id:id ...) inner-rhs:expr #|Expr[G/∆O/∆L][(values tI ...)]|#] ...)
              pre-guard:expr #|Expr[G/∆O/∆L/∆I][Boolean]|#
              post-guard:expr #|Expr[G/∆O/∆L/∆I][Boolean]|#
              (loop-arg:expr #|Expr[G/∆O/∆L/∆I][tL]|# ...)]
             ;; where ∆O = outer-id:tO ... ...
             ;;       ∆L = loop-id:tL ... 
             ;;       ∆I = inner-id:tI ... ...
             ;; ∆O, ∆L, ∆I tell us what new bindings are introduced
             ;; by those parts of the ECR.
             )))
