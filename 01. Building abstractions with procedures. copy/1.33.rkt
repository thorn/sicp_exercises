#lang racket
#|

You can obtain an even more general version of accumulate (Exercise 1.32) by introducing the notion of a filter on the terms to be combined. That is, combine only those terms derived from values in the range that satisfy a specified condition. The resulting filtered-accumulate abstraction takes the same arguments as accumulate, together with an additional predicate of one argument that specifies the filter. Write filtered-accumulate as a procedure. Show how to express the following using filtered-accumulate:

a. the sum of the squares of the prime numbers in the interval a to b (assuming that you have a prime? predicate already written)

b. the product of all the positive integers less than n that are relatively prime to n (i.e., all positive integers i < n such that GCD(i,n) = 1).

|#

; prime? definition
(define (next-divisor test-divisor)
  (if (= test-divisor 2) 3
        (+ test-divisor 2)))

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next-divisor test-divisor)))))
(define (divides? a b) (= (remainder b a) 0))

(define (square x) (* x x))
(define (prime? n) (= (smallest-divisor n) n))

; a. filtered-accumulate
(define (filtered-accumulate combiner null-value term a next b predicate?)
  (if (> a b)
      null-value
      (combiner
       (if (predicate? a) (term a) null-value)
       (filtered-accumulate combiner null-value term (next a) next b predicate?))))

(define (sum-of-squares-prime a b)
  (define (square n) (* n n))
  (define (inc n) (+ n 1))
  (filtered-accumulate + 0 square a inc b prime?))
(sum-of-squares-prime 0 100)