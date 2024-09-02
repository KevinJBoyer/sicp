#lang sicp

; pg 367

(define (list-has-one-element? some-list) (null? (cdr some-list)))

(define (make-table)
  (let ((local-table (list '*table*)))
    
    (define (assoc key records)
      (cond ((null? records) false)
            ((eq? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    
    (define (lookup-key key)
            (let ((record
                   (assoc key (cdr local-table))))
              (if record (cdr record) false)))

    (define (lookup keys)
      (if (list-has-one-element? keys)
          (lookup-key (car keys))
          (let ((record
                 (assoc (car keys) (cdr local-table))))
            (if record
                (((cdr record) 'lookup-proc) (cdr keys))
                false))))
    
    (define (insert-key! key value)
            (let ((record
                   (assoc key (cdr local-table))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! local-table
                            (cons (cons key value)
                                  (cdr local-table)))))
      'ok)

    (define (insert! keys value)
      (if (list-has-one-element? keys)
          (insert-key! (car keys) value)
          (let ((record
                 (assoc (car keys) (cdr local-table))))
            (cond (record (((cdr record) 'insert-proc!) (cdr keys) value))
                (else 
                 (insert-key! (car keys) (make-table))
                 (insert! keys value))))))
      
    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))




(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))


; testing

(put (list 3) 'a)
(get (list 3))

(put (list 4) 'b)
(get (list 4))

(put (list 1 2) 'c)
(get (list 1 2))

(put (list 1 3) 'd)
(get (list 1 3))

(put (list 2 5 9) 'f)
(get (list 2 5 9))

(put (list 2 5 9) 'g)
(get (list 2 5 9))