#lang sicp

(define (make-table same-key?)
  (let ((local-table (list 'table)))
    (define (assoc key records)
      (cond ((null? records) #f)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup key-1 key-2)
      (display local-table)
      (newline)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define my-equal? eq?)
(define operation-table (make-table my-equal?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put 'hello 'world 'value)
(put 'foo 'bar 'new-value)
(get 'hello 'world)
