#lang racket

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (last-pair items)
  (if (null? items)
      (error "The list must contain at least one element")
      (list-ref items (- (length items) 1))))

(last-pair (list 23 72 149 34))
