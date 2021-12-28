#lang sicp
(define (log el)
  (display el)
  (newline))

(define empty-table (list 'table))

(define (make-table same-key?)
  (let ((local-table (list 'table)))
    (define (assoc key records)
      (cond ((null? records) #f)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    
    (define (lookup keys)
      (define (iter keys table)
        (if (null? keys)
            (cdr table)
            (iter (cdr keys) (assoc (car keys) (cdr table)))))

      (if (null? keys)
          (error "Key list cannot be empty -- INSERT")
          (iter keys local-table)))

    (define (add-subtable table key value) (set-cdr! table  
                                                     (cons (cons key value)
                                                           (cdr table))))
    (define (change-value item value) (set-cdr! (car item) value)) 

    (define (insert! keys value)
      (define (iter keys current-level)
        (let ((key (car keys))
              (rest-of-keys (cdr keys))
              (subtable (assoc (car keys) (cdr current-level))))
          (if (null? rest-of-keys)
              (if subtable
                  (change-value subtable value) 
                  (add-subtable current-level key value))
              (if subtable
                  (iter rest-of-keys subtable)
                  (begin (add-subtable current-level key '())
                         (iter rest-of-keys (cadr current-level))))))
        'ok)
      (if (null? keys)
          (error "Key list cannot be empty -- INSERT")
          (iter keys local-table)))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define my-equal? eq?)
(define operation-table (make-table my-equal?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put (list 'hello 'world) 'my-value)
(put (list 'hello 'foo) 'bar)
(get (list 'hello))
(get (list 'hello 'world))
(get (list 'hello 'foo))