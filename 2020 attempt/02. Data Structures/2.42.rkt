#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(define empty-board (list))

(define (adjoin-position row queens) (cons row queens))

(define (safe? layout)
  (define (iter old-queens n)
    (if (null? old-queens)
        true
        (let ((new (car layout))
              (old (car old-queens)))
          (if (or (= new old)
                  (= new (+ old n))
                  (= new (- old n)))
              false
              (iter (cdr old-queens) (+ n 1))))))
    (iter (cdr layout) 1))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         safe?
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
; (flatmap
;  (lambda (new-row)
;    (map (lambda (rest-of-queens)
;           (adjoin-position new-row k rest-of-queens))
;         (queen-cols (- k 1))))
;  (enumerate-interval 1 board-size))
(queens 4)