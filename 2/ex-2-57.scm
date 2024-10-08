#lang sicp

(define (deriv exp var) 
  (cond ((number? exp) 0) 
        ((variable? exp) 
         (if (same-variable? exp var) 1 0)) 
        ((sum? exp) 
         (make-sum (deriv (addend exp) var) 
                   (deriv (augend exp) var)))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-product (make-exponentiation
                                      (base exp)
                                      (- (exponent exp) 1))
                                     (deriv (base exp) var))))
        ((product? exp) 
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
        (else (list '+ a1 a2)))) 
  
(define (make-product m1 m2) 
  (cond ((or (=number? m1 0) (=number? m2 0)) 0) 
        ((and (number? m1) (= m1 1)) m2) 
        ((and (number? m2) (= m2 1)) m1) 
        ((and (number? m1) (number? m2)) (* m1 m2)) 
        (else (list '* m1 m2)))) 
   
(define (=number? exp num) 
  (and (number? exp) (= exp num))) 
  
(define (sum? x) 
  (and (pair? x) (eq? (car x) '+))) 
  
(define (addend s) (cadr s)) 

(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s)))) 
  
(define (product? x) 
  (and (pair? x) (eq? (car x) '*))) 
  
(define (multiplier p) (cadr p)) 
  
(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cons '* (cddr p))))

(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))

(define (base exp) (cadr exp))
(define (exponent exp) (caddr exp))
(define (make-exponentiation base exponent)
  (cond ((and (number? exponent) (= exponent 0)) 1)
        ((and (number? exponent) (= exponent 1)) base)
        (else (list '** base exponent))))

(deriv '(* y 8 x x) 'x)