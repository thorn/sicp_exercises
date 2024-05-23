#lang lazy

(define (square x) (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.0001))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x))) ; endless loop as arguments are precomputed when
                                            ; using applicative order of argument evaluation
                                            ; #lang lazy uses normal order and the function
                                            ; works fine

(define (my-sqrt x)
  (sqrt-iter 1.0 x))

(my-sqrt 2)