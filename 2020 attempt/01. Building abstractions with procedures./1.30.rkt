#lang racket

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (simpson f a b n) 
  (define h (/ (- b a) n))
  (define (g k)
    (define (c k)
      (cond ((= k 0) 1)
            ((= k n) 1)
            ((even? k) 2)
            (else 4)))
    (* (c k) (f (+ a (* k h)))))
  (define (inc k) (+ k 1))
  (/ (* (sum g 0 inc n) h) 3))

(define (cube x) (* x x x))
(simpson cube 0 1 1000)