#lang racket
(define (power a b) 
  (exp (* b (log a))))

(define tolerance 0.00001)

(define (fixed-point f first-guess) 
  (define (close-enough? v1 v2) 
    (< (abs (- v1 v2)) tolerance)) 
  (define (try guess) 
    (let ((next (f guess))) 
         (if (close-enough? guess next) 
             next 
             (try next)))) 
  (try first-guess))

(define (average a b) 
  (/ (+ a b) 2.0))

(define (average-damp f) 
  (lambda (x) (average x (f x))))

(define (square x) (* x x))
(define (compose f g) (lambda (x) (f (g x))))

(define (repeated f n)
  (if (> n 1)
      (compose (repeated f (- n 1)) f)
      f))

(define (nth-root n x damp-count) 
  (define (f y) (/ x (power y (- n 1)))) 
  (fixed-point ((repeated average-damp damp-count) f) 1.0))

(nth-root 16 2 4) 