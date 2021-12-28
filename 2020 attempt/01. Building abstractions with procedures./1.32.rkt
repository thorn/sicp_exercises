#lang racket

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))
  
(define (accumulate-recursive combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-recursive combiner null-value term (next a) next b))))
      
(define (product term a next b)
  (accumulate * 1 term a next b))

(define (++ a) (+ a 1))
(define (square a) (* a a))

(define (factorial a)
  (define (identity x) x)
  (product identity 1 ++ a))
(factorial 10)

(define (pi n)
  (define (next-pi a)
    (+ a 2))
  (/ (* 2 (product square 4 next-pi n))
     (* (+ n 1) (product square 3 next-pi n))))

(pi 10)