#lang racket/base

(require racket/match
         plot/pict
         pict)
;(plot-new-window? #t)

(provide plot-seq-comparison)

(define filepaths (list "srcs/dyn1.txt" "srcs/dyn2.txt" "srcs/dyn3.txt" "srcs/lists.txt"
                        "srcs/wo-opt.txt" "srcs/opt.txt" "srcs/hand-opt-ecr.txt")
                  #;(directory-list "srcs" #:build? #t))

(define unscaled-plot-seq-comparison
  (let* ([srcs (map (lambda (filepath)
                      (define in (open-input-file filepath))
                      (let ([src (read in)])
                        (close-input-port in)
                        src))
                    filepaths)]
         [do/sequence-opts (sort (map car srcs) (lambda (x y)
                                                  (let ([vx (car x)]
                                                        [vy (car y)])
                                                    (or (> vx vy)
                                                        (and (= vx vy)
                                                             (string>? (cdr x) (cdr y))))))
                                 #:key (lambda (x) (cons (car (cdaadr x)) (car x))))]
         [w/o-do/sequence (cadar srcs)]
         [n (add1 (length filepaths))])
    (plot (append (for/list ([do/sequence-opt (in-list do/sequence-opts)]
                             [i (in-range 1 n)])
                    (discrete-histogram
                     (let ([val (car (cdaadr do/sequence-opt))])
                       (list (list val val)))
                     ;#:invert? #t
                     ;#:skip n
                     #:x-min i
                     #:color i #:line-color i
                     #:label (car do/sequence-opt)))
                  (list (discrete-histogram
                         (let ([val (car (cdaadr w/o-do/sequence))])
                           (list (list val val)))
                         ;#:invert? #t
                         ;#:skip n
                         #:x-min n
                         #:color n #:line-color n
                         #:label (car w/o-do/sequence))))
          #:y-max 2000
          #:x-label #f ; "Solutions"
          #:y-label "Time (milliseconds)"
          #:legend-anchor 'top-right
          #:width 900
          #:height 350
          #:out-file "plot.pdf"
          #:out-kind 'pdf
          )))

(define plot-seq-comparison
  (scale unscaled-plot-seq-comparison 0.45))
