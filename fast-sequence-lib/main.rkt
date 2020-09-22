#lang racket

(require "private/do-sequence.rkt"
         "private/fast-sequence-map.rkt"
         "private/fast-sequence-filter.rkt")

(provide (all-from-out "private/do-sequence.rkt")
         (all-from-out "private/fast-sequence-map.rkt")
         (all-from-out "private/fast-sequence-filter.rkt"))

(module+ private-for-testing
  (require (submod "private/do-sequence.rkt" private-for-testing)
           (submod "private/fast-sequence-filter.rkt" private-for-testing))
  (provide do/sequence2
           in-nullary-relation
           (for-syntax bind-clause
                       when-clause
                       expanded-clause-record
                       nest
                       merge
                       fast-sequence-filter-transformer)))