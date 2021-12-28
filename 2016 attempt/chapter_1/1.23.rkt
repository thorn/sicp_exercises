;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |1.23|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require sicp)

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (begin (newline)
         (display n)
         (start-prime-test n (runtime))))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      false))

(define (report-prime elapsed-time)
  (begin
    (display " *** ")
    (display elapsed-time)))

(define (prime-test start count)
  (if (<= count 0)
      (newline)
      (begin
        (if (timed-prime-test start)
            (prime-test (+ start 1) (- count 1))
            (prime-test (+ start 1) count))
        )))

(prime-test 1000 3)
(prime-test 10000 3)
(prime-test 100000 3)
(prime-test 1000000 3)