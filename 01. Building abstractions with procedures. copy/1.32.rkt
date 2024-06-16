#lang racket
#|
a. Show that sum and product (Exercise 1.31) are both special cases of a still more general notion called accumulate that combines a collection of terms, using some general accumulation function:
           (accumulate combiner null-value term a next b)

accumulate takes as arguments the same term and range specifications as sum and product, together with a combiner procedure (of two arguments) that specifies how the current term is to be combined with the accumulation of the preceding terms and a null-value that specifies what base value to use when the terms run out. Write accumulate and show how sum and product can both be defined as simple calls to accumulate.

b. If your accumulate procedure generates a recursive process, write one that generates an iterative process. If it generates an iterative process, write one that generates a recursive process.
|#

; a)
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (product-squares a b)
  (define (square n) (* n n))
  (define (inc n) (+ n 1))
  (product square a inc b))

(product-squares 1 5) ; 1 * 4 * 9 * 25 = 14400

; b)
(define (accumulate-iter combiner null-value term a next b)
  (define (iter result a)
    (if (> a b)
        result
        (iter (combiner (term a) result) (next a))))
  (iter null-value a))

(define (product-iter term a next b)
  (accumulate-iter * 1 term a next b))

(define (pi-approximation-iter precision)
  (define (inc n) (+ n 1))
  (define (term n)
    (* (/ (* 2 n)
          (- (* 2 n) 1))
       (/ (* 2 n)
          (+ (* 2 n) 1))))
  (* 2 (product-iter term 1.0 inc precision)))
(pi-approximation-iter 1000000.0)