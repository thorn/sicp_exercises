#lang racket

; Абстрагируйте свой ответ на упражнение 2.30, получая процедуру tree-map, так, чтобы square-
; tree можно было определить следующим образом
(define (square x) (* x x))

(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))
       tree))

(define (square-tree tree) (tree-map square tree))

(define my-tree (list 1
                      (list 2 (list 3 4) 5)
                      (list 6 7)))
(square-tree my-tree)
; (1 (4 (9 16) 25) (36 49))


