#lang racket

#|
Define a procedure reverse that takes a list as argument and returns a list of the same elements in reverse order:

(reverse (list 1 4 9 16 25))
(25 16 9 4 1)

|#


(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (last-pair items)
  (list (list-ref items (- (length items) 1))))

(define (reverse items)
  (define (reverse-iter items reversed)
    (if (null? items)
        reversed
        (reverse-iter (cdr items) (cons (car items) reversed))))
  (reverse-iter items (list)))
  
(reverse (list 1 2 3 4 5))