#lang racket

(define (divides? a b) (= (remainder a b) 0))
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> test-divisor n) n)
        ((divides? n test-divisor) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (prime? n) (= (smallest-divisor n) n))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (fold-right op init seq)
  (accumulate op init seq))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

; Определите процедуру unique-pairs, которая, получая целое число n, порождает последова-
; тельность пар (i, j), таких, что 1 ≤ j < i ≤ n. С помощью unique-pairs упростите данное выше
; определение prime-sum-pairs.

(define (unique-pairs n)
  (flatmap (lambda (i)
           (map (lambda (j) (list i j))
                (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))             



(define (permutations s)
  (if (null? s)
      (list null)
      (gather s)))

(define (gather s)
  (flatmap (lambda (x)
             (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(prime-sum-pairs 6)