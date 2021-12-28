#lang racket

(define (factor base value)
  (define (factor-iter value counter)
    (if (= (remainder value base) 0)
        (factor-iter (/ value base) (+ counter 1))
        counter))
  (factor-iter value 0))

(define (power base value)
  (define (power-iter counter product)
    (if (= counter 0)
        product
        (power-iter (- counter 1) (* base product))))
  (power-iter value 1))

(define (cons a b)
  (* (power 2 a) (power 3 b)))

(define (car z)
  (factor 2 z))

(define (cdr z)
  (factor 3 z))

(car (cons 3 5))
(cdr (cons 3 5))
