#lang racket

; Предположим, что мы представляем векторы v = (vi ) как последовательности чисел, а матрицы
; m = (mij ) как последовательности векторов (рядов матрицы). Например, матрица
; представляется в виде последовательности ((1 2 3 4) (4 5 6 6) (6 7 8 9)). Имея такое
; представление, мы можем использовать операции над последовательностями, чтобы кратко выра-
; зить основные действия над матрицами и векторами. Эти операции (описанные в любой книге по
; матричной алгебре) следующие:


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define s (list (list 1 2 3 4)
                (list 4 5 6 6)
                (list 6 7 8 9)))

(define v1 (list 2 2 2 2))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose mat)
  (accumulate-n cons null mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))


(dot-product v1 v1)
(matrix-*-vector s v1)
(transpose s)
(matrix-*-matrix s (transpose s))