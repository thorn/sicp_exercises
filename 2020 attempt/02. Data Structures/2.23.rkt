#lang racket

(define (square x) (* x x))

(define (square-list items)
  (if (null? items)
      null
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list-iter items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer (list (square (car things)))))))
  (iter items null))


(define (square-list-2 items)
  (map (lambda (x) (* x x)) items))

(define (for-each f items)
  (define (iter things)
    (if (null? things)
        true
        (and (f (car things)) (iter (cdr things)) )))
  (iter items))

(for-each (lambda (x) (display x)(newline) )
          (list 57 321 88))
