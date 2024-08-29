#lang racket
#|
Exercise 2.17: Define a procedure last-pair that returns
the list that contains only the last element of a given (nonempty) list:

(last-pair (list 23 72 149 34))
(34)
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

(define a (list 1 2 3 4 5))
(last-pair a)