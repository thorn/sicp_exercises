#lang racket


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

;;====== Installing coercions========================================
(define (install-coercion-package)
  (define (scheme-number->complex n)
    (make-complex-from-real-imag (contents n) 0))

  (define (scheme-number->rational n)
    (make-rational (contents n) 1))

  (define (complex->rational  n )  ;;в моем мире все комплексные числа приводятся к рациональной 1/2
    (make-rational 1 2))           ;;для тестирования это ОК

  (put-coercion 'scheme-number 'rational scheme-number->rational)
  (put-coercion 'scheme-number 'complex scheme-number->complex)
  (put-coercion 'complex 'rational complex->rational)
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))
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

;;======= Обобщенные арифметические процедуры
(define (add x y) (apply-generic 'add x y))
(define (add3 x y z) (apply-generic 'add3 x y z))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))


;;======== Начнем с установки пакета для работы с обычными числами
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))

  (put 'add3 '(scheme-number  scheme-number scheme-number)
       (lambda (x y z) (tag (+ x y z))))

  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))

  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))

  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))

  (put 'make 'scheme-number
       (lambda (x) (tag x)))

  (put 'equ? '(scheme-number scheme-number) =)

  (put '=zero? '(scheme-number) (lambda (x) (= x 0)))

  'done)

;;======= Rational package
(define (install-rational-package)
  ; внутренние процедуры
  (define (numer x) (car x))
  (define (denom x) (cdr x))

  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))

  (define (add-rat x y)
    (make-rat (+  (* (numer x) (denom y))
                  (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (-  (* (numer x) (denom y))
                  (* (numer y) (denom x)))
              (* (denom x) (denom y))))

  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

  (define (equ-rat? x y)
    (= (* (numer x) (denom y))
       (* (denom x) (numer y))))

  (define (=zero-rat? x) (= (numer x) 0)) 

  ;; интерфейс к остальной системе
  (define (tag x) (attach-tag 'rational x))

  (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))


  (put 'add3 '(rational rational rational)
       (lambda (x y z) (tag  (add-rat z (add-rat x y)))))

  (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational) equ-rat?)
  (put '=zero? '(rational) =zero-rat?)
  'done)

(define (square x) (* x x))
;;======= Rect and Polar packages
(define (install-rectangular-package)
  ;; внутренние процедуры
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+  (square (real-part z))
              (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))

  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; интерфейс к остальной системе
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ; внутренние процедуры
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  ;; интерфейс к остальной системе
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)


;;========  Complex package ====================
(define (install-complex-package)
  ;; процедуры, импортируемые из декартова
  ;; и полярного пакетов
  (install-rectangular-package)
  (install-polar-package)

  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))

  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; внутренние процедуры
  (define (add-complex z1 z2)
    (make-from-real-imag  (+ (real-part z1) (real-part z2))
                          (+ (imag-part z1) (imag-part z2))))

  (define (sub-complex z1 z2)
    (make-from-real-imag  (- (real-part z1) (real-part z2))
                          (- (imag-part z1) (imag-part z2))))

  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))

  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  (define (equ-complex? z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))

  (define (=zero-complex? z)
    (= (magnitude z) 0))
    

  ;; интерфейс к остальной системе
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ? '(complex complex) equ-complex?)
  (put '=zero? '(complex) =zero-complex?)
  'done)

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))


(define (make-scheme-number d)
  ((get 'make 'scheme-number) d))
(install-coercion-package)
(install-complex-package)
(install-rational-package)
(install-scheme-number-package)



;;====Ну и собственно apply-generic =====================================================================
;;
(define (apply-generic op . args)
  (define (app-recur op tagList coerced_args) ;; tagList - список тагов. Уменьшаеся с каждой итерацией.
    ;; Все аргументы приводим к типу (car tagList)
    (let ((type-tags (map type-tag coerced_args)))
      (let ((proc (get op type-tags)))
        (if proc
            (apply proc (map contents coerced_args))
            (if (not (null? tagList))
                (let ((type1 (car tagList)))
                  (app-recur op (cdr tagList)
                             (map (lambda (el)
                                    (let ((coercion (get-coercion (type-tag el) type1 )))
                                      (if coercion     ;;если операции приведения нет, то оставляем эл.  изменений
                                          (coercion el)
                                          el)))
                                  args))
                  )
                (error "Нет метода для этих типов"  (list op type-tags))
                )
            );end if proc
        )
      )
    ) ;app-recur

  (app-recur op (map type-tag args) args)
  )
;;===========================

(define r1
  (add   (make-rational 4 8)
         (make-rational 2 3)))
;; r1 == 7/6

(define c1
  (add  (make-complex-from-real-imag 4 8)
        (make-complex-from-real-imag 4 8)))

;; c1 == 8 + i*16
(define n1 (make-scheme-number 45))
(define n2 (make-scheme-number 8))

(add3 r1 n2 n1)
(add3 n1 n2 r1)
(add3 n2 n1 r1)
(add3 n2 r1 n1)
(add3 c1 r1 n1)

(equ? (make-scheme-number 1) (make-scheme-number 1))
(equ? (make-scheme-number 1) (make-scheme-number 2))
(define r3 (make-rational 4 8))
(equ? (make-rational 8 9) (add r3 r3))
(equ? (make-rational 8 8) (add r3 r3))
(equ? (make-complex-from-real-imag 2 1) (make-complex-from-real-imag 2 1))
(equ? (make-complex-from-real-imag 4 8) (make-complex-from-real-imag 4 8))

(=zero? (make-scheme-number 0))
(=zero? (make-scheme-number 1))
(=zero? (make-rational 0 1))
(=zero? (make-rational 1 1))
(=zero? (make-complex-from-real-imag 0 0))
(=zero? (make-complex-from-real-imag 4 8))