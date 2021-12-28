#lang racket
(define (even? x) (= (remainder x 2) 0))
(define (fast-mul b n)
  (fast-mul-iter b n 1))
(define (square x) (* x x))

(define (fast-mul-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-mul-iter (square b) (/ n 2) a))
        (else (fast-mul-iter b (- n 1) (* a b)))))

(fast-mul 2 6)
(fast-mul 2 7)
(fast-mul 2 8)