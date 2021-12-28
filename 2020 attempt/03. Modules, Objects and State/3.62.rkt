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

(define tangent-series (div-series sine-series cosine-series))
(show-stream tangent-series 10)