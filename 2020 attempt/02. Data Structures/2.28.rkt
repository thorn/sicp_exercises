#lang racket

;Напишите процедуру fringe, которая берет в качестве аргумента дерево (представленное в ви-
;де списка) и возвращает список, элементы которого — все листья дерева, упорядоченные слева
;направо.


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

(define (fringe tree)
  (cond ((null? tree) tree)
        ((pair? tree) (append (fringe (car tree)) (fringe (cdr tree))))
        (else (list tree))))

(define x (list (list 1 2) (list 3 4)))
(fringe x)
;(1 2 3 4)
(fringe (list x x))
;(1 2 3 4 1 2 3 4)
