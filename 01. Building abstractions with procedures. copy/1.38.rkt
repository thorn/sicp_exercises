#lang racket
#|
In 1737, the Swiss mathematician Leonhard Euler published a memoir De Fractionibus Continuis, which included a continued fraction expansion for e − 2, where e is the base of the natural logarithms. In this fraction, the Ni are all 1, and the Di are successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, . . .. Write a program that uses your cont-frac procedure from Exercise 1.37 to approximate e, based on Euler’s expansion.
|#

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1)
              (/ (n i)
                 (+ (d i) result)))))
  (iter k 0))

(define (n i) 1)
(define (d i)
  (if (= (remainder i 3) 2)
      (* 2 (/ (+ i 1) 3))
      1))
(define (my-exp) (+ 2 (cont-frac-iter n d 20000.0)))
(my-exp)