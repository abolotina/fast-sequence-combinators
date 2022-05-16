#lang info

;; ========================================
;; pkg info

(define collection "fast-sequence")
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
