#lang racket

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (product-recursive term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-recursive term (next a) next b))))

(define (++ a) (+ a 1))
(define (square a) (* a a))

(define (factorial a)
  (define (identity x) x)
  (product-recursive identity 1 ++ a))
(factorial 10)

(define (pi n)
  (define (next-pi a)
    (+ a 2))
  (/ (* 2 (product square 4 next-pi n))
     (* (+ n 1) (product square 3 next-pi n))))

(pi 10)