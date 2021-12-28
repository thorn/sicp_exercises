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

(square-list (list 1 2 3 4))
(square-list-iter (list 1 2 3 4))
(square-list-2 (list 1 2 3 4))