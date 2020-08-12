#lang racket/base

(require racket/match
         racket/sequence)

(provide in-concat-sequences1
         in-concat-sequences2
         in-concat-sequences)

;; in-concat-sequences : Sequence[Sequence[X]] -> Sequence[X]
(define (in-concat-sequences1 seq-seq)
  (sequence-fold (lambda (seq acc) (sequence-append acc seq))
                 null
                 seq-seq))

(define (in-concat-sequences2 seq-seq)
  (apply sequence-append (sequence->list seq-seq)))

;; If X... = String Number Symbol
;; then (List X ...) = (List String Number Symbol), eg (list "apple" 12 'north)

;; (Listof X)
;; eg, (Listof Number), eg (list 1 2 3), (list), (list 1 5 18 22)

;; sequence-generate* : Sequence[X...] -> Result[X...]
;; Result[X...] = (values (U #f (List X ...)) (-> Result[X...]))

;; (sequence-generate* seq-seq)
;; : Result[Sequence[X...]]
;; = (values (U #f (List Sequence[X...])) (-> Result[Sequence[X...]]))

;; in-concat-sequences : Sequence[Sequence[X...]] -> Sequence[X...]
(define (in-concat-sequences seq-seq)
  (make-do-sequence
   (lambda ()
     ;; Position_outer[X...] = (Pair Boolean (-> Position_outer[X...])))
     ;; Position_inner[X...] = (Pair (U #f (List X ...) (-> Position_inner[X...])))
     ;; Position[X...] = (Pair Position_outer[X...] Position_inner[X...])
     
     (define-syntax-rule (get-outer-pos pos) (car pos))
     (define-syntax-rule (get-outer-rest pos) (cdar pos))
     (define-syntax-rule (get-inner-elt pos) (cadr pos))
     (define-syntax-rule (get-inner-rest pos) (cddr pos))

     (define (outer-is-not-done? pos) (caar pos))
     
     (define (next-position pos)
       (cond
         [(get-inner-elt pos)
          (loop-inner (get-outer-pos pos) (get-inner-rest pos))]
         [(outer-is-not-done? pos)
          (loop-outer (get-outer-rest pos))]
         [else
          (outer-is-done)]))

     (define (loop-outer get-outer-result)
       (let-values ([(outer-elt outer-rest)
                     (get-outer-result)])
         (cond
           [outer-elt
            (loop-inner (cons #t outer-rest)
                        (lambda () (sequence-generate* (car outer-elt))))]
           [else
            (outer-is-done)])))
     
     (define (loop-inner outer-pos get-inner-result)
       (let*-values ([(inner-elt inner-rest) (get-inner-result)]
                     [(next-pos) (cons outer-pos
                                       (cons inner-elt inner-rest))])
         (cond
           [inner-elt next-pos]
           [else (loop-outer (get-outer-rest next-pos))])))
     
     (define initial-position
       (loop-outer (lambda () (sequence-generate* seq-seq))))
     
     (define (outer-is-done)
       (define (done) (error 'done))
       (cons (cons #f done) (cons #f done)))

     (define (get-element pos)
       (apply values (get-inner-elt pos)))
     
     (values
      get-element         ;; Position[X...] -> (values X ...)
      next-position       ;; Position[X...] -> Position[X...]
      initial-position    ;; Position[X...]
      outer-is-not-done?  ;; Position[X...] -> Boolean
      #f
      #f
      ))))

#|
   (for ([i (in-range 10)])
     (for ([j (in-list (range i))])
       (f i j)))
   ;; Position_i = Nat
   ;; Position_j = List

   =>

   (for ([ijpos (in-range/list-nested 10 (lambda (i) (range i)))])
     (f (?? ijpos) (?? ijpos)))
   ;; Position = (cons Nat List)
|#
