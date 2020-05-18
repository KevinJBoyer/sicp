#lang sicp

(define (deriv exp var) 
  (cond ((number? exp) 0) 
        ((variable? exp) 
         (if (same-variable? exp var) 1 0)) 
        ((sum? exp)
         (display "sum: ") (display (addend exp)) (display " | ") (display (augend exp)) (newline)
         (make-sum (deriv (addend exp) var) 
                   (deriv (augend exp) var)))
        ((exponentiation? exp)
         (display "exp: ") (display (base exp)) (display " | ") (display (exponent exp)) (newline)
         (make-product (exponent exp)
                       (make-product (make-exponentiation
                                      (base exp)
                                      (- (exponent exp) 1))
                                     (deriv (base exp) var))))
        ((product? exp) 
         (display "mul: ") (display (multiplier exp)) (display " | ") (display (multiplicand exp)) (newline)
         (make-sum 
          (make-product (multiplier exp) 
                        (deriv (multiplicand exp) var)) 
          (make-product (deriv (multiplier exp) var) 
                        (multiplicand exp)))) 
        (else 
         (error "unknown expression type -- DERIV" exp)))) 
  
(define (variable? x) (symbol? x)) 
  
(define (same-variable? v1 v2) 
  (and (variable? v1) (variable? v2) (eq? v1 v2))) 
  
(define (make-sum a1 a2) 
  (cond ((and (number? a1) (= a1 0)) a2) 
        ((and (number? a2) (= a2 0)) a1) 
        ((and (number? a1) (number? a2)) (+ a1 a2)) 
        (else (list a1 '+ a2)))) 
  
(define (make-product m1 m2) 
  (cond ((or (=number? m1 0) (=number? m2 0)) 0) 
        ((and (number? m1) (= m1 1)) m2) 
        ((and (number? m2) (= m2 1)) m1) 
        ((and (number? m1) (number? m2)) (* m1 m2)) 
        (else (list m1 '* m2)))) 
   
(define (=number? exp num) 
  (and (number? exp) (= exp num))) 
  
(define (sum? x)
  (and (pair? x)
       (memq '+ x)))
  
(define (addend s) (singlify (before '+ s)) )

(define (augend s) (singlify (cdr (memq '+ s))) )
  
(define (product? x) 
  (and (pair? x)
       (not (memq '+ x))
       (memq '* x)))
  
(define (multiplier p) (singlify (before '* p)) )
  
(define (multiplicand p) (singlify (cdr (memq '* p))))

(define (exponentiation? exp) 
  (and (pair? exp)
       (not (memq '+ exp))
       (not (memq '* exp))
       (memq '** exp)))

(define (base exp) (singlify (before '** exp)))
(define (exponent exp) (singlify (cdr (memq '** exp))))
(define (make-exponentiation base exponent)
  (cond ((and (number? exponent) (= exponent 0)) 1)
        ((and (number? exponent) (= exponent 1)) base)
        (else (list base '** exponent))))

(define (before item list)
  (if (or (null? list) (eq? (car list) item))
      nil
      (cons (car list) (before item (cdr list)))))

(define (singlify list) (if (null? (cdr list)) (car list) list))

(deriv '(3 * x + 5 + x ** 2 * y + 9) 'x)
