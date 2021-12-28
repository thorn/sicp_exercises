;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |1.3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require racket/pretty)

(define (square a) (* a a))
(define (sum-of-squares a b) (+ (square a) (square b)))

(define (biggest-sum-square a b c)
  (if (> a b)
      (if (> b c)
          (sum-of-squares a b)
          (sum-of-squares a c))
      (if (> a c)
          (sum-of-squares a b)
          (sum-of-squares b c))
  )
)
(pretty-print (biggest-sum-square 1 3 5))
(pretty-print (biggest-sum-square 1 5 3))
(pretty-print (biggest-sum-square 3 1 5))
(pretty-print (biggest-sum-square 3 5 1))
(pretty-print (biggest-sum-square 5 1 3))
(pretty-print (biggest-sum-square 5 3 1))