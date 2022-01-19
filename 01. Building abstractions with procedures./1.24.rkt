#lang racket

(define (square x) (* x x))
(define runtime current-milliseconds)

; fast-prime?
(define (expmod base exp m) 
  (cond ((= exp 0) 1) 
        ((even? exp) 
         (remainder (square (expmod base (/ exp 2) m)) 
                    m)) 
        (else 
         (remainder (* base (expmod base (- exp 1) m)) 
                    m))))

(define (fermat-test n) 
  (define (try-it a) 
    (= (expmod a n n) a)) 
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times) 
  (cond ((= times 0) true) 
        ((fermat-test n) (fast-prime? n (- times 1))) 
        (else false)))
; ------------

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time) 
  (if (fast-prime? n 1000000) 
      (report-prime n (- (runtime) start-time)) 
      false))

(define (report-prime n elapsed-time) 
  (display n) 
  (display " *** ") 
  (display elapsed-time) 
  (newline) 
  true)

(define (search-for-primes n count)
  (when (> count 0)
      (if (timed-prime-test n)
          (search-for-primes (+ n 1) (- count 1))
          (search-for-primes (+ n 1) count))))
(search-for-primes 1000 10)
(newline)
(search-for-primes 10000 10)
(newline)
(search-for-primes 100000 10)
; averages are chaning by constant
