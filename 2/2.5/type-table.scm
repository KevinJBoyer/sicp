#lang sicp

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      ;'ok
      )    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation - TABLE" m))))
    dispatch))

;;;;;

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(#%provide get)
(#%provide put)

;;;;;

(define (type-tag x)
  (cond ((number? x) 'integer)
        ((eq? #false x) 'bool)
        ((eq? #true x) 'bool)
        (else (car x))))
(define (contents x) (if (number? x) x (cdr x)))
(define (attach-tag tag item)
  (if (and (number? item) (not (equal? tag 'real)))
      item
      (cons tag item)))

(#%provide contents)
(#%provide type-tag)
(#%provide attach-tag)