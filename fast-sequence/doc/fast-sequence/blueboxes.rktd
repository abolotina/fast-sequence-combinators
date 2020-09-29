231
((3) 0 () 1 ((q lib "fast-sequence/main.rkt")) () (h ! (equal) ((c form c (c (? . 0) q fast-sequence-map)) q (0 . 5)) ((c form c (c (? . 0) q do/sequence)) q (213 . 12)) ((c form c (c (? . 0) q fast-sequence-filter)) q (97 . 5))))
syntax
(fast-sequence-map f seq-expr ...+)
 
  f : procedure?
  seq-expr : sequence?
syntax
(fast-sequence-filter pred seq-expr)
 
  pred : (-> any/c ... boolean?)
  seq-expr : sequence?
syntax
(do/sequence (binding-or-when-chunk ...) body ...+)
 
binding-or-when-chunk = binding-clause ...+
                      | when-clause ...+
                         
       binding-clause = [(id ...) seq-expr]
                      | [id seq-expr]
                         
          when-clause = #:when guard-expr
 
  seq-expr : sequence?
