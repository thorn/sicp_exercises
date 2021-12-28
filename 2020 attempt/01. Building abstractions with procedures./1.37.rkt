#lang racket

(define tolerance 0.00000000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y) (/ (+ x y) 2))

(define golden-ratio (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

(define (cont-frac n d k)
  (define (iter i)
    (/ (n i) (+ (d i)
                (if (< i k)
                    (iter (+ i 1))
                    0))))
  (iter 1))

(/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 100))

(define (count-iterations function value tolerance) 
  (define (try k) 
    (define (close-enough? a b) 
      (< (abs (- a b)) tolerance)) 
    (if (close-enough? (function k) value) 
        k 
        (try (+ k 1)))) 
  (try 1))
(define (golden-ratio-function k) 
  (/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k)))
(count-iterations golden-ratio-function golden-ratio 0.00001) 
