#lang racket

(require racket/trace)

(define (my-remainder a b)
  (remainder a b))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (my-remainder a b))))

(trace my-remainder)
(gcd 206 40)

; Applicative order: 4 times
; >(my-remainder 206 40)
; >(my-remainder 40 6)
; >(my-remainder 6 4)
; >(my-remainder 4 2)

; Normal order: 18 times
; (gcd 206 40)


; (if (= (remainder (remainder 40 
;                              (remainder 206 
;                                         40)) 
;                   (remainder (remainder 206 
;                                         40) 
;                              (remainder 40 
;                                         (remainder 206 
;                                                    40)))) 
;        0) 
;     (remainder (remainder 206 
;                           40) 
;                (remainder 40 
;                           (remainder 206 
;                                      40))) 
;     (gcd (remainder (remainder 206 
;                                40) 
;                     (remainder 40 
;                                (remainder 206 
;                                           40))) 
;          (remainder (remainder 40 
;                                (remainder 206 
;                                           40)) 
;                     (remainder (remainder 206 
;                                           40) 
;                                (remainder 40 
;                                           (remainder 206 
;                                                      40))))))
; > 7 + 7
; ----------


; (if (= 0 
;        0) 
;     (remainder (remainder 206 
;                           40) 
;                (remainder 40 
;                           (remainder 206 
;                                      40))) 
;     (gcd (remainder (remainder 206 
;                                40) 
;                     (remainder 40 
;                                (remainder 206 
;                                           40))) 
;          (remainder (remainder 40 
;                                (remainder 206 
;                                           40)) 
;                     (remainder (remainder 206 
;                                           40) 
;                                (remainder 40 
;                                           (remainder 206 
;                                                      40)))))) 
; >14

; (remainder (remainder 206 
;                       40) 
;            (remainder 40 
;                       (remainder 206 
;                                  40)))
; > 14 + 4 = 18