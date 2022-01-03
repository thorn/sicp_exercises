#lang racket

; calculate elements of Pascal's triangles
(define (p-element row col)
  (cond ((> col row) (error "THERE IS NO SUCH ELEMENT COL: " col  "ROW:" row))
        ((or (= col 1) (= row col)) 1)
        (else (+ (p-element (- row 1) (- col 1)) (p-element (- row 1) col)))))

(p-element 3 2) ; => 2
(p-element 5 2) ; => 4
(p-element 5 3) ; => 6
; (p-element 2 3) ; => Error