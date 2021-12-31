#lang racket

(define (inc a) (+ a 1))
(define (dec a) (- a 1))

(define (plus-recursive a b)
  (if (= a 0)
      bЫ
      (inc (plus-recursive (dec a) b))))

(define (plus-iterative a b)
  (if (= a 0)
      b
      (plus-iterative (dec a) (inc b))))

(plus-recursive 3 4)
(plus-iterative 3 4)