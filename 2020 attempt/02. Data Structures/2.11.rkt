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

(add-interval (make-interval 6.12 7.48) (make-interval 6.12 7.48))
(sub-interval (make-interval 6.12 7.48) (make-interval 6.12 7.48))

(div-interval (make-interval 2 8) (make-interval 2 8))