#lang racket

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (let ((next (improve guess)))
      (if (good-enough? guess next)
          next
          (iter next))))
  iter)

(define tolerance 0.000001)
(define (square x) (* x x))
(define (average a b) (/ (+ a b) 2))

(define (fixed-point f first-guess)
  (define (good-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  ((iterative-improve good-enough? f) first-guess))

(define (average-damp f) (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(sqrt 4)
(sqrt 5)
