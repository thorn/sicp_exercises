#lang racket

(define (add x y) (apply-generic 'add x y))
(define (mul x y) (apply-generic 'mul x y))

;;=========== put/get procedures====================================
;;
(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) #f))
;;============ put/get coercion procedures =========================

(define *coercion-table* (make-hash))

(define (put-coercion type1 type2 proc)
  (hash-set! *coercion-table* (list type1 type2) proc))

(define (get-coercion type1 type2)
  (hash-ref *coercion-table* (list type1 type2) #f))

;;=========== Polynomial package ===================================
(define (install-polynomial-package)
  ;; Internal procedures
  ;; Definition of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? v) (symbol? v))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polynoms with different variables -- ADD-POLY"
               (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polynoms with different variables -- MUL-POLY"
               (list p1 p2))))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2)) (adjoin-term t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2)) (adjoin-term t2 (add-terms L1 (rest-terms L2))))
                   (else (adjoin-term
                          (make-term (order t1) (add (coeff t1) (coeff t2)))
                          (add-terms (rest-terms L1)
                                     (rest-terms L2)))))))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (=zero? x) 
    (define (poly? x) 
      (pair? x)) 
    (cond ((number? x) (= x 0)) 
          ((poly? x) false) 
          (else (error "Unknown type")))) 
 
  ;; Interface
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? 'polynomial =zero?)
  'done)

;;======================================================================
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Некорректные помеченные данные -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Некорректные помеченные данные -- CONTENTS" datum)))

;;================= apply-generic ===================================
(define (coerce arg type)
  (if (eq? (type-tag arg) type)
      arg
      (let ((arg->type (get-coercion (type-tag arg) type)))
        (if arg->type
            (arg->type arg)
            false))))

(define (coerce-all args type)
  (if (null? args)
      '()
      (let ((first (coerce (car args) type))
            (rest (coerce-all (cdr args) type)))
        (if (and first rest)
            (cons first rest)
            false))))

(define (apply-with-coercion op args types)
  (if (null? types)
      (error "No method for these types APPLY-WITH-COERCION" (list op types))
      (let ((type (car types)))
        (let ((coerced-args (coerce-all args type)))
          (if coerced-args
              (apply apply-generic (cons op coerced-args))
              (apply-with-coercion op args (cdr types)))))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (apply-with-coercion op args type-tags)))))

(install-polynomial-package)