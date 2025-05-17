#lang info

;; ========================================
;; pkg info

(define pkg-name "fast-sequence")
(define pkg-desc "Macros for fast sequences")
(define version "0.0")
(define collection "fast-sequence")
(define license
  '(Apache-2.0 OR MIT))

(define deps
  '(["base" #:version "7.4"]
    "fast-sequence-lib"
    "rackunit-lib"))
(define build-deps
  '("sandbox-lib"
    "racket-doc"
    "scribble-lib"))
(define implies
  '("fast-sequence-lib"))

;; ========================================
;; collect info

(define name "fast-sequence")
(define scribblings '(["fast-sequence.scrbl" ()]))
