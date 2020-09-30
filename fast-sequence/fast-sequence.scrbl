#lang scribble/manual
@(require racket/sandbox
          scribble/example
          (for-label racket
                     fast-sequence))
@(define ref-src
   '(lib "scribblings/reference/reference.scrbl"))

@(define my-evaluator
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string])
     (make-evaluator 'racket
                     #:requires '(fast-sequence))))

@title{Fast Sequence}

The library provides efficient sequence operations that have good
performance when used inside a @racket[for] clause.

@defmodule[fast-sequence]

@section{Basic Concepts}

The operations provided by this library have high performance when
applied to a @italic{fast sequence}.  A fast sequence uses syntactic
information about the shape of a sequence to apply compiler
optimizations, such as inlining and using specialized functions for
extracting data. Examples of fast sequences are applications of a
sequence macro, such as @racket[in-range], @racket[in-list], etc.
(see @secref[#:doc ref-src]{sequences}).

For a way of defining new fast sequences, see
@racket[define-sequence-syntax].

A @italic{slow sequence}—all other sequences, such as applications of
@racket[list], @racket[vector], etc.—provides to the compiler a
generic interface for data accession and has lesser performance at
run-time.

An @italic{element} of a sequence can be a single value or it can
consist of multiple values.

@section{Motivating Example}

As a running example, consider the following task: calculate a sum and
a product of squares of even positive numbers up to 10000.

There are several ways of doing it. The simplest one uses
@racket[range] and a composition of @racket[filter] and @racket[map]
to define a sequence of squares of even positive numbers up to 10000
and then reuses it:

@examples[#:eval my-evaluator #:label #f
 (define (squares-of-evens-up-to-10000)
   (map sqr (filter even? (range 1 10000))))

 (for/sum ([x (squares-of-evens-up-to-10000)])
   x)

 (void
  (code:comment "a very long number")
  (for/product ([x (squares-of-evens-up-to-10000)])
    x))
 ]

A downside of this solution is that it eagerly allocates a big
intermediate list. We can fix it by using the lazy @racket[in-range]
sequence instead, with appropriate changes to the used operations. We
only need to change the sequence definition; that does not affect the
rest of the solution. The improved version is

@examples[#:eval my-evaluator #:label #f
(define (squares-of-evens-up-to-10000)
  (sequence-map
   sqr
   (sequence-filter
    even?
    (in-range 1 10000))))
 ]

But what about its performance? When we use specialized sequence
constructors like @racket[in-range], the Racket compiler can run them
very fast because it uses inlining and specialization. And functions
@racket[sequence-filter] and @racket[sequence-map] return some generic
sort of a sequence as a result. Therefore, we lose the performance.

Another way of doing it is to apply @racket[even?] and @racket[sqr] in
a @racket[#:when] clause or directly in the body of a loop:

@examples[#:eval my-evaluator #:label #f
 (for/sum ([x (in-range 1 10000)]
            #:when (even? x))
   (sqr x))

 (void
  (code:comment "a very long number")
  (for/product ([x (in-range 1 10000)])
    (if (even? x)
        (sqr x)
        1)))
 ]

This will run fast, but the code was nicer in the previous
solution. Here we lose abstraction of working with sequences, and we
can no longer define a reusable sequence.

Using fast sequence operations provided by this library, we can take
the best of these two worlds:

@examples[#:eval my-evaluator #:label #f
(define-sequence-rule (squares-of-evens-up-to-10000)
  (fast-sequence-map
   sqr
   (fast-sequence-filter
    even?
    (in-range 1 10000))))
 ]

In this code, @racket[fast-sequence-filter] and
@racket[fast-sequence-map] may be considered as versions of
@racket[sequence-filter] and @racket[sequence-map] that, being applied
to a specialized sequence, still run fast. Note that as like as
specialized sequences, their applications are efficient only when
applied in a clause of a @racket[for] loop or its variants, so it is
necessary to define a fast sequence form instead of merely using
@racket[define]. The @racket[define-sequence-rule] from is similar to
@racket[define-syntax-rule], but it defines a sequence macro, which
can be efficiently used in a @racket[for] loop clause.

@section{Fast Sequence Operations}

@defform[(fast-sequence-map f seq-expr ...+)
         #:contracts ([f procedure?]
                      [seq-expr sequence?])]{
Returns a fast sequence form whose elements are results of applying
@racket[f] to the elements of @racket[seq-expr]s.  The number of
elements is determined by the length of the shortest
@racket[seq-expr]. The number of @racket[seq-expr]s must match the
number of arguments that @racket[f] accepts, and all the elements of
each sequence must be single values.

The @racket[fast-sequence-map] form is similar to
@racket[sequence-map] provided by @racketmodname[racket/sequence] but
it accepts multiple sequence expressions, whereas
@racket[sequence-map] accepts a single sequence. When used directly in
a @racket[for] loop clause, @racket[fast-sequence-map] has better
performance than @racket[sequence-map]. The best performance is
provided when @racket[seq-expr] is a fast sequence.

@examples[#:eval my-evaluator
          (for/list ([(x) (fast-sequence-map
                           add1
                           (in-list '(1 2 3 4 5)))])
            x)
          (for/list ([(x) (fast-sequence-map
                           symbol->string
                           (in-vector #(a b c)))])
            x)]
}

@defform[(fast-sequence-filter pred seq-expr)
         #:contracts ([pred (-> any/c ... boolean?)]
                      [seq-expr sequence?])]{
Returns a fast sequence form whose elements are the elements of
@racket[seq-expr] that satisfy the predicate function @racket[pred].

When used directly in a @racket[for] loop clause,
@racket[fast-sequence-filter] has better performance than
@racket[sequence-filter] provided by
@racketmodname[racket/sequence]. The best performance is provided when
@racket[seq-expr] is a fast sequence.

@examples[#:eval my-evaluator
          (for/list ([c (fast-sequence-filter
                         char-alphabetic?
                         (in-string "a, b, c"))])
            c)
          (for/list ([(k v) (fast-sequence-filter
                             (lambda (k v) (odd? v))
                             (in-hash (hash 'a 1 'b 2 'c 3 'd 4 'e 5)))])
            (list k v))]
}

@defform[(do/sequence (binding-or-when-chunk ...) body ...+)
         #:grammar
         [(binding-or-when-chunk (code:line binding-clause ...+)
                                 (code:line when-clause ...+))
          (binding-clause [(id ...) seq-expr]
                          [id seq-expr])
          (when-clause (code:line #:when guard-expr))]
         #:contracts ([seq-expr sequence?])]{
When used directly in a @racket[for] loop, returns a fast sequence
form whose elements are results of evaluating the last @racket[body]
on each iteration of the loop. Using outside a @racket[for] loop is
forbidden; in that case, it raises an error.

The @racket[do/sequence] form provides two kinds of @italic{clauses}:
a @racket[binding-clause] and a @racket[when-clause], which specify
the number of elements of the sequence. These clauses have the same
meaning as the corresponding clauses of the @racket[for] loop. Other
kinds of @racket[for] clauses (e.g., @racket[#:break]) are not
supported.

Clauses are grouped into @italic{chunks} of clauses of the same
kind. Within the same chunk, expressions, @racket[seq-expr]s or
@racket[guard-expr]s, are evaluated left-to-right and elements of the
sequences are extracted in parallel, one per iteration of the loop,
and stored in locations generated for the corresponding
@racket[id]s. The @racket[id]s are bound in all expressions in the
succeeding chunks and in the @racket[body]s.

If there are more than one chunk of clauses then the @racket[for] loop
performs nested iterations for sequences in each succeeding
chunk. (This behavior is the same as the behavior of the @racket[for]
loop's clauses.)

With no clauses, @racket[do/sequence] returns a sequence with a single
element.

The @racket[do/sequence] form is as expressive as both
@racket[fast-sequence-map] and @racket[fast-sequence-filter], but it
additionally allows iterating over nested sequences.

When @racket[do/sequence] returns a sequence whose elements are all
single values, it is equivalent to using @racket[for/list] in place of
@racket[do/sequence], except that @racket[do/sequence] does not
construct a list.

The @racket[do/sequence] form provides the best performance when it is
used directly in a @racket[for] loop clause and all
@racket[seq-expr]s are fast sequences.

@examples[#:eval my-evaluator
          (for/list ([(x) (do/sequence () 13)])
            x)
          (for/list ([x (do/sequence ([x (in-list '(1 2 3))]
                                      [y (in-list '(a b c))]
                                      #:when (odd? x))
                          (list x y))])
            x)
          (for ([(x y) (do/sequence ([x (in-list '(1 2 3))]
                                     #:when (odd? x)
                                     [y (in-list '(a b c))])
                         (values x y))])
            (printf "~a " (list x y)))
          (for ([x (do/sequence ([x (in-list '((1 2) (3 7) () (5 6)))]
                                 #:when #t
                                 [y (in-list x)]
                                 #:when (even? y))
                     y)]
                [y (for/list ([x (in-list '((1 2) (3 7) () (5 6)))]
                              #:when #t
                              [y (in-list x)]
                              #:when (even? y))
                     y)])
            (printf "~a ~a\n" x y))]
}

@section{Defining New Sequence Forms}

@defform[(define-sequence-rule (id . pattern) template)
         #:contracts ([template sequence?])]{
The @racket[define-sequence-rule] form binds @racket[id] as a sequence
macro that matches a single pattern.  Using @racket[(id . pattern)] is
only allowed directly in a clause of a @racket[for] loop (or its
variants).  A @racket[template] must be a sequence expression. It
provides better performance when @racket[template] is a fast sequence
expression.

@examples[#:eval my-evaluator
          (define-sequence-rule (in-hash-set vec-of-lsts)
            (do/sequence ([(x) (in-vector vec-of-lsts)]
                          #:when #t
                          [(y) (in-list x)])
              y))]
}
