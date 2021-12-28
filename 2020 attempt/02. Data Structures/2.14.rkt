#lang racket

(define (make-interval a b) (cons a b))

(define (lower-bound interval) (min (car interval) (cdr interval)))
(define (upper-bound interval) (max (car interval) (cdr interval)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ((xl (lower-bound x))
        (xu (upper-bound x))
        (yl (lower-bound y))
        (yu (upper-bound y)))
    (cond ((>= xl 0)
           (cond ((>= yl 0) (make-interval (* xl yl) (* xu yu)))
                 ((<= yu 0) (make-interval (* xu yl) (* xl yu)))))
          ((<= xu 0)
           (cond ((>= yl 0) (make-interval (* xl yu) (* xu yl)))
                 ((<= yu 0) (make-interval (* xu yu) (* xl yl)))
                 (else (make-interval (* xl yu) (* xl yl)))))
          (else
           (cond ((>= yl 0) (make-interval (* xl yu) (* xu yu)))
                 ((<= yu 0) (make-interval (* xu yl) (* xl yl)))
                 (else (make-interval (min (* xl yu) (* xu yl))
                                      (max (* xl yl) (* xu yu)))))))))

(define (div-interval x y)
  (if (< (* (lower-bound y) (upper-bound x)) 0)
      (error "Division by internval that spans zero" y)
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define (make-center-width c w) (make-interval (- c w) (+ c w)))
(define (center i) (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i) (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent av pc)
  (let ((width (/ (* av pc) 100.0)))
    (make-center-width av width)))

(define (percent i)
  (* 100 (/ (width i) (center i))))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
(define a-interval (make-center-percent 100 5))
(define b-interval (make-center-percent 1000 5))

(par1 a-interval a-interval)
(par2 a-interval a-interval)