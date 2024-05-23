;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |1.05|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

(test 0 (p)) ; endless loop thus applicative normal: arguments first, unfold the body later

