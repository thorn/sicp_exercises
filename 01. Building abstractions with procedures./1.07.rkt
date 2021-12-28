#lang racket

(define (square x) (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess prev-guess)
  (< (abs (- 1 (/ guess prev-guess))) 0.0000000001))

(define (sqrt-iter guess prev-guess x)
  (if (good-enough? guess prev-guess)
          guess
          (sqrt-iter (improve guess x) guess x)))

(define (my-sqrt x)
  (sqrt-iter 1.0 4 x))

(my-sqrt 10000000000000000)