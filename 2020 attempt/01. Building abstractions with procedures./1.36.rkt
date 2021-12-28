#lang racket

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess iteration) 
    (display iteration) 
    (display ":t") 
    (display guess) 
    (newline) 
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next (+ iteration 1)))))
  (try first-guess 1))
(define (f x) (/ (log 1000) (log x)))
(define (g x) 
  (define (average x y) (/ (+ x y) 2)) 
  (average x (f x)))

(fixed-point f 2)
(fixed-point g 2)