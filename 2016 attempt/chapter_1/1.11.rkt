;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |1.11|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1)) (f (- n 2)) (f (- n 3))))))

(define (f-i n)
  (if (< n 3) n
      (f-iter n 0 1 2)))

(define (f-iter n a b c)
  (if (= n 0) a
      (f-iter (- n 1) b c (+ a b c))))

(f-i 30)
(f 30)
