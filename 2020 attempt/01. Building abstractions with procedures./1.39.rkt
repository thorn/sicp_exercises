#lang racket

(define (cont-frac n d k)
  (define (iter i)
    (/ (n i) (+ (d i)
                (if (< i k)
                    (iter (+ i 1))
                    0))))
  (iter 1))

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (- (* x x))))
             (lambda (i) (+ i (- i 1.0)))
             k))
(tan-cf 180 1000)