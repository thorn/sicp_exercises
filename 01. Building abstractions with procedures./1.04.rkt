;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |1.04|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (square x) (* x x))
(define (sum-of-squares a b) (+ (square a) (square b)))

(define (sum-of-squares-max-two a b c)
  (if (> a b)
      (if (> b c)
          (sum-of-squares a b)
          (sum-of-squares a c))
      (if (> a c)
          (sum-of-squares b a)
          (sum-of-squares b c))))
(sum-of-squares-max-two 1 2 3)
(sum-of-squares-max-two 1 3 2)
(sum-of-squares-max-two 3 1 2)
(sum-of-squares-max-two 3 2 1)
(sum-of-squares-max-two 2 1 3)