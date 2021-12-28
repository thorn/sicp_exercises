#lang racket

(define (filtered-accumulate predicat combiner null-value term a next b)
  (if (> a b)
      null-value
      (if (filter a)
          (combiner (term a)
                    (accumulate-recursive combiner null-value term (next a) next b))
          (filtered-accumulate combiner null-value term (next a) next b filter))))
