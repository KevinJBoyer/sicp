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

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;;;;;;;;;;;

(define (variable? x) (symbol? x)) 
  
(define (same-variable? v1 v2) 
  (and (variable? v1) (variable? v2) (eq? v1 v2))) 

(define (=number? exp num) 
  (and (number? exp) (= exp num)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (deriv-sum operands var)
  (list '+ (deriv (car operands) var) (deriv (cadr operands) var)))

(define (deriv-mult operands var)
  (list '+
        (list '* (car operands) (deriv (cadr operands) var))
        (list '* (deriv (car operands) var) (cadr operands))))

; swap these for part d
(put 'deriv '+ deriv-sum)
(put 'deriv '* deriv-mult)

(define (deriv-expo operands var)
  (list '*
        (cadr operands)
        (list '*
              (list '**
                    (car operands)
                    (- (cadr operands) 1))
              (deriv (car operands) var))))

(put 'deriv '** deriv-expo)

;(deriv '(+ (* x x) (* 3 x)) 'x)
(deriv '(* 2 (** x 3)) 'x)
