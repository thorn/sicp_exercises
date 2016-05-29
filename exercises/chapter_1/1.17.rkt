;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |1.17|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (halve x) (/ x 2))
(define (double x) (* x 2))

(define (fast-m a b)
  (fast-m-iter 0 a b))

(define (fast-m-iter acc a b)
  (cond ((= b 0) acc)
        ((even? b) (fast-m-iter acc (double a) (halve b)))
        (else (fast-m-iter (+ acc a) a (- b 1)))))

(fast-m 12 3)
(fast-m 12 4)
