;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |1.3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; Определите процедуру, которая принимает в качестве аргументов три числа и возвращает сумму квадратов двух больших из них

(define (square a) (* a a))
(define (sum-of-squares a b) (+ (square a) (square b)))

(define (sum-of-largest-squares a b c)
  (cond ((and (>= a b) (>= b c)) (sum-of-squares a b))
        ((and (>= a b) (<= b c)) (sum-of-squares a c))
        (else (sum-of-squares b c)))
)

(sum-of-largest-squares 4 3 2)
(sum-of-largest-squares 4 2 3)
(sum-of-largest-squares 3 2 4)
(sum-of-largest-squares 3 3 4)
(sum-of-largest-squares 3 2 2)
(sum-of-largest-squares 3 3 3)