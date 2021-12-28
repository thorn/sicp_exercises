#lang sicp

;;(define stream-null? null?)
;;(define the-empty-stream '())
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map (cons proc (map stream-cdr argstreams))))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define (show m) (display m) (newline))
(define (show-stream s n)
  (if (= n 0)
      (show "Done")
      (begin
        (show (stream-car s))
        (show-stream (stream-cdr s) (- n 1)))))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1) (stream-cdr s2)))))))))


(define (integers-from n)
  (cons-stream n (integers-from (+ n 1))))
(define integers (integers-from 1))

(define (integrate-series s)
              (stream-map /  s integers))

(define cosine-series 
  (cons-stream 1 (stream-map - (integrate-series sine-series)))) 
(define sine-series 
  (cons-stream 0 (integrate-series cosine-series))) 

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                                        (mul-series (stream-cdr s1) s2))))
(define (invert-unit-series series)
  (define inverted-unit-series
    (cons-stream 1 (scale-stream (mul-series (stream-cdr series) inverted-unit-series) -1)))
  inverted-unit-series)

(define (div-series nums dems) 
  (mul-series nums 
              (invert-unit-series dems))) 

(define (stream-limit stream tolerance)
  (define (good-enough? a b)
    (< (abs (- a b)) tolerance))
  (define (iter s)
    (let ((a (stream-ref s 0))
          (b (stream-ref s 1)))
      (if (good-enough? a b)
          b
          (iter (stream-cdr s)))))
  (iter stream))

(define (average x y) (/ (+ x y) 2))
(define (sqrt-improve guess x)
  (average guess (/ x guess)))
(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
               (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream 
   (list (stream-car s) (stream-car t))
   (interleave
     (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (merge-weighted s1 s2 weight)
  (let ((cars1 (stream-car s1))
        (cars2 (stream-car s2)))
    (if (<= (weight cars1) (weight cars2))
        (cons-stream cars1 (merge-weighted (stream-cdr s1) s2 weight))
        (cons-stream cars2 (merge-weighted s1 (stream-cdr s2) weight)))))

(define (weighted-pairs s1 s2 weight)
  (cons-stream (list (stream-car s1) (stream-car s2))
               (merge-weighted (stream-map (lambda (x) (list (stream-car s1) x))
                                           (stream-cdr s2))
                               (weighted-pairs (stream-cdr s1) (stream-cdr s2) weight)
                               weight)))
        

(define weight1 (lambda (x) (+ (car x) (cadr x))))
(define pairs1 (weighted-pairs integers integers weight1))

(define weight2 (lambda (x) (+ (* 2 (car x)) (* 3 (cadr x)) (* 5 (car x) (cadr x)))))   
(define (divide? x y) (= (remainder y x) 0))
(define stream235
  (stream-filter (lambda (x) (not (or (divide? 2 x) (divide? 3 x) (divide? 5 x))))
                 integers))
(define pairs2 (weighted-pairs stream235 stream235 weight2))
(show-stream pairs1 10)
(show-stream pairs2 10)