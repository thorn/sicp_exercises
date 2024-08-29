#lang racket
#|
Show that we can represent pairs of nonnegative integers using only numbers and arithmetic operations if we represent the pair a and b as the integer that is the product 2^a3^b . Give the corresponding definitions of the procedures cons, car, and cdr.
|#

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car a)
  (define (cons-iter a res)
    (if (= (remainder a 2) 1)
        res
        (cons-iter (/ a 2) (+ res 1))))
  (cons-iter a 0))

(define (cdr a)
  (define (cdr-iter a res)
    (if (= (remainder a 3) 0)
        (cdr-iter (/ a 3) (+ res 1))
        res))
  (cdr-iter a 0))

(define a (cons 4 8))
(car a)
(cdr a)