#lang racket

; Определите процедуру square-tree, подобную процедуре square-list из упражнения 2.21. А
; именно, square-tree должна вести себя следующим образом:

; Определите square-tree как прямо (то есть без использования процедур высших порядков), так
; и с помощью map и рекурсии.
(define (square x) (* x x))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))

(define (square-tree-simple tree)
  (cond ((null? tree) null)
        ((list? tree) (cons (square-tree-simple (car tree))
                            (square-tree-simple (cdr tree))))
        (else (square tree))))

(define my-tree (list 1
                      (list 2 (list 3 4) 5)
                      (list 6 7)))
(square-tree my-tree)
(square-tree-simple my-tree)
; (1 (4 (9 16) 25) (36 49))
