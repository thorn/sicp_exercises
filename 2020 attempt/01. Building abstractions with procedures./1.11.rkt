#lang racket
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (f (- n 2)) (f (- n 3)))))

(define (f-iter n)
  (f-iter-internal 2 1 0 n))

(define (f-iter-internal a b c count)
  (if (= count 0)
      c
      (f-iter-internal (+ a b c) a b (- count 1))))
(f 2)
(f 3)
(f 4)
(f 5)
(f 6)

(f-iter 2)
(f-iter 3)
(f-iter 4)
(f-iter 5)
(f-iter 6)