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

; Напишите процедуру, которая находит все такие упорядоченные тройки различных положительных
; целых чисел i, j и k, меньших или равных данному целому числу n, сумма которых равна данному
; числу s.

(define (unique-trios n)
  (flatmap (lambda (i)
           (flatmap (lambda (j)
                  (map (lambda (k) (list i j k))
                         (enumerate-interval 1 (- j 1))))
                (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (trios-equal-to n s)
  (filter (lambda (t) (= (accumulate + 0 t) s))
            (unique-trios n)))

(trios-equal-to 5 8)