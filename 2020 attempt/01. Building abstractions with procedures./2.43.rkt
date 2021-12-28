#lang racket
(define (square x) (* x x))
(define (compose f g) (lambda (x) (f (g x))))

(define (repeated f n)
  (if (> n 1)
      (compose (repeated f (- n 1)) f)
      f))

((repeated square 2) 5)
