#lang racket

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
  (if (null? items)
      (error "The list must contain at least one element")
      (list-ref items (- (length items) 1))))

(define (reverse items)
  (define (reverse-iter result source)
    (if (null? source)
        result
        (reverse-iter (cons (car source) result) (cdr source))))

  (reverse-iter (list) items))
    
(last-pair (list 23 72 149 34))
(reverse (list 23 72 149 34))