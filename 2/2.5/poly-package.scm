#lang sicp

(#%require "type-table.scm")
(#%require "arithmetic-system.scm")

(#%require "terms.scm")

(define (make-polynomial var terms) ((get 'make 'poly) var terms))

(define (install-poly-package)
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  (define same-variable? eq?)
  
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: ADD-POLY" (list p1 p2))))

  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1) (neg-termlist (term-list p2))))
        (error "Polys not in same var: SUB-POLY" (list p1 p2))))
  
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))
  
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((resultant (div-terms (term-list p1) (term-list p2))))
          (list (tag (make-poly (variable p1) (car resultant)))
                (tag (make-poly (variable p1) (cadr resultant)))))
        (error "Polys not in same var: DIV-POLY" (list p1 p2))))

  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (gcd-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: GCD-POLY" (list p1 p2))))
  
  (define (tag x) (attach-tag 'poly x))

  ; external
  (put '=zero? '(poly)
       (lambda (p) (empty-termlist? (term-list p))))
  
  (put 'add '(poly poly)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(poly poly)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(poly poly)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(poly poly)
       (lambda (p1 p2) (div-poly p1 p2)))
  (put 'gcd '(poly poly)
       (lambda (p1 p2) (tag (gcd-poly p1 p2))))
  
  (put 'make 'poly
       (lambda (var terms) (tag (make-poly var terms)))))



(install-poly-package)

(#%provide make-polynomial)