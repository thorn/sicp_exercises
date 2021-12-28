#lang racket
(define (even? x) (= (remainder x 2) 0))
(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (fast-mul a b)
  (cond ((= b 0) 0)
        ((= b 1) a)
        ((even? b) (fast-mul (double a) (halve b)))
        (else (+ a (fast-mul a (- b 1))))))


(fast-mul 2 6)
(fast-mul 2 7)
(fast-mul 2 8)
(fast-mul 4 8)
(fast-mul 3 18)