#lang racket
#|
Modify your reverse procedure of exercise 2.18 to produce a deep-reverse procedure that takes a list as argument and returns as its value the list with its elements reversed and with all sublists deep-reversed as well. For example,

(define x (list (list 1 2) (list 3 4)))

x
((1 2) (3 4))

(reverse x)
((3 4) (1 2))

(deep-reverse x)
((4 3) (2 1))
|#

(define (reverse items)
  (define (reverse-iter items reversed)
    (if (null? items)
        reversed
        (reverse-iter (cdr items) (cons (car items) reversed))))
  (reverse-iter items (list)))

(define (deep-reverse items)
  (define (reverse-iter items reversed)
    (if (null? items)
        reversed
        (reverse-iter
         (cdr items)
         (cons
          (if (pair? (car items))
              (deep-reverse (car items))
              (car items))
          reversed))))
  (reverse-iter items (list)))
  

(define x (list (list 1 2) (list 3 (list 5 6) 4)))

x
; ((1 2) (3 4))

(reverse x)
; ((3 4) (1 2))

(deep-reverse x)
; ((4 3) (2 1))