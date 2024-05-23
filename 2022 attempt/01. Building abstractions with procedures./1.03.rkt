;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |1.03|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (square x) (* x x))
(define (sum-of-squares a b) (+ (square a) (square b)))

(define (sum-of-squares-max-two a b c)
  (if (a > b)
      (if (b > c)
          (sum-of-squares a b)
          (sum-of-squares a c))
      (if (a > c)
          (sum-of-squares b a)
          (sum-of-squares b c))))
(sum-of-squares-max-two 1 2 3)