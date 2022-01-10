#lang racket

(define (even? x) (= (remainder x 2) 0))
(define (double x) (+ x x))
(define (halve x) (floor (/ x 2)))

(define (fast-mult a b)
  (cond ((= b 1) a)
        ((even? b) (double (fast-mult a (halve b))))
        (else (+ a (fast-mult a (- b 1))))))

(fast-mult 3 8)
(fast-mult 3 9)
(fast-mult 3 10)