#lang racket
(define (square x) (* x x))
(define (compose f g) (lambda (x) (f (g x))))

(define (repeated f n)
  (if (> n 1)
      (compose (repeated f (- n 1)) f)
      f))

(define (smooth f)
  (define dx 0.00001)
  (define (average a b c) (/ (+ a b c) 3))
  (lambda (x)
    (average (f (- x dx))
             (f x)
             (f (+ x dx)))))

(define (n-smoothed f n)
  ((repeated (lambda (g) (smooth g)) n) f))

((n-smoothed square 2) 1.0)