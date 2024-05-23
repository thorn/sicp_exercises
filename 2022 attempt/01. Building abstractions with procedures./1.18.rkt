#lang racket

(define (even? x) (= (remainder x 2) 0))
(define (double x) (+ x x))
(define (halve x) (floor (/ x 2)))

(define (fast-mult a b)
  (define (fast-mult-iter acc a b)
    (cond ((= b 0) acc)
          ((even? b) (fast-mult-iter acc (double a) (halve b)))
          (else (fast-mult-iter (+ acc a) a (- b 1)))))
  (fast-mult-iter 0 a b))

(fast-mult 3 8)
(fast-mult 3 9)
(fast-mult 3 10)