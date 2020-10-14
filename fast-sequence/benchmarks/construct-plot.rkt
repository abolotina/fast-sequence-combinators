#lang racket/base

(require racket/match
         plot)
(plot-new-window? #t)

(define filepaths (list "srcs/wo-opt.txt" "srcs/opt2.txt" "srcs/opt1.txt"
                        "srcs/opt3.txt" "srcs/opt1+3.txt")
                  #;(directory-list "srcs" #:build? #t))

(let* ([srcs (map (lambda (filepath)
                    (define in (open-input-file filepath))
                    (let ([src (read in)])
                      (close-input-port in)
                      src))
                  filepaths)]
       [do/sequence-opts (map car srcs)]
       [w/o-do/sequence (cadar srcs)]
       [n (add1 (length filepaths))])
  (plot (append (for/list ([do/sequence-opt (in-list do/sequence-opts)]
                           [i (in-range 1 n)])
                  (discrete-histogram
                   (cadr do/sequence-opt)
                   #:invert? #t
                   ;#:skip n
                   ;#:x-min i
                   #:color i #:line-color i
                   #:label (car do/sequence-opt)))
                (list (discrete-histogram
                       (cadr w/o-do/sequence)
                       #:invert? #t
                       ;#:skip n
                       ;#:x-min n
                       #:color n #:line-color n
                       #:label (car w/o-do/sequence))))
          #:x-max 180
          #:y-label "Solutions"
          #:x-label "Time (milliseconds)"
          #:legend-anchor 'bottom-right
          #:width 1500
          #:height 1000
          #:out-file "optimizations.pdf"
          #:out-kind 'pdf))
