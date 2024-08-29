#lang racket
#|
Define a constructor make-center-percent that takes a center and a percentage tolerance and produces the desired interval. You must also define a selector percent that produces the percentage tolerance for a given interval. The center selector is the same as the one shown above.
|#

(define (make-interval a b) (cons a b))
(define (lower-bound i) (min (car i) (cdr i)))
(define (upper-bound i) (max (car i) (cdr i)))

(define (make-center-percent center percentage)
  (make-interval (- center (* (/ center 100) percentage))
                 (+ center (* (/ center 100) percentage))))

(define a (make-center-percent 2 50))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(center a)