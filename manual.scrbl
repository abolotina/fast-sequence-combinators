#lang scribble/manual
@(require (for-label racket
                     "main.rkt"))
@(define ref-src
   '(lib "scribblings/reference/reference.scrbl"))
 
@title{Fast Sequence}

The fast-seq-combinators library provides efficient sequence combinators that have good performance when
used inside the @racket[for] loop.

@defmodule[fast-sequence]

@section{Basic Concepts}

The functions provided by this library provide high performance when applied to a @italic{fast sequence form}.
A fast sequence form is an application of a sequence constructor, such as @racket[in-range], @racket[in-list], etc.
(see @secref[#:doc ref-src]{sequences}). It uses syntactic information about the shape of a sequence to apply
compiler optimizations, such as inlining and using specialized functions for extracting data.
(For a way of defining new fast sequence forms, see @racket[define-sequence-syntax].)

In contrast, a @italic{slow sequence form}—all other sequences, such as applications of @racket[list], @racket[vector],
etc.—provides to the compiler a generic interface for data accession and has lesser performance at run-time. Also,
sequence operations provided by @racketmodname[racket/sequence], such as @racket[sequence-map] or @racket[sequence-filter],
return a slow sequence form, even when applied to a fast sequence.

A @italic{fast sequence operation} applied to a fast sequence form returns a fast sequence form.

@section{Motivation}



@section{Fast Sequence Operations}

@defform[(do/sequence (binding-or-when-chunk ...) body ...+)
         #:grammar
         [(binding-or-when-chunk (code:line binding-clause ...+)
                                 (code:line when-clause ...+))
          (binding-clause [(id ...) seq-expr]
                          [id seq-expr])
          (when-clause (code:line #:when guard-expr))]
         #:contracts ([seq-expr sequence?])]{
Returns a fast sequence form whose elements are results of evaluating the last body on each iteration of
the @racket[for] loop. It provides two kinds of @italic{clauses}: a @racket[binding-clause] and a @racket[when-clause],
which specify the number of elements of the sequence.
}
