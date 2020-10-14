#lang racket/base

(require "private/do-sequence.rkt"
         "private/do-sequence-wo-opt.rkt"
         "private/do-sequence-opt1.rkt"
         "private/do-sequence-opt2.rkt"
         "private/do-sequence-opt3.rkt"
         "private/fast-sequence-map.rkt"
         "private/fast-sequence-filter.rkt"
         (for-syntax syntax/parse
                     racket/base))

(provide (all-from-out "private/do-sequence.rkt")
         (all-from-out "private/fast-sequence-map.rkt")
         (all-from-out "private/fast-sequence-filter.rkt")
         define-sequence-rule
         do/sequence-w/o-opt
         do/sequence-opt1
         do/sequence-opt2
         do/sequence-opt3)

(define-syntax-rule (define-sequence-rule (id . pattern) template)
  (define-sequence-syntax id
    (lambda (stx)
      (raise-syntax-error #f "only allowed in a fast sequence context" stx))
    (lambda (stx)
      (syntax-parse stx
        [[(id1 (... ...)) (_ . pattern)]
         (for-clause-syntax-protect #'[(id1 (... ...)) template])]))))

(module+ private-for-testing
  (require (submod "private/do-sequence.rkt" private-for-testing)
           (submod "private/fast-sequence-filter.rkt" private-for-testing))
  (provide in-nested
           in-when
           (for-syntax bind-clause
                       when-clause
                       expanded-clause-record
                       nest
                       merge
                       fast-sequence-filter-transformer)))