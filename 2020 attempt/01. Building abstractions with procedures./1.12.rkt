#lang racket

(define (pascal row col)
  (cond ((or (= col 0) (= row col)) 1)
        ((and (> row col) (> col 0)) (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col)))))

(pascal 4 3)