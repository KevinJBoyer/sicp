#lang sicp

(#%require "type-table.scm")

(define (make-scheme-number n) ((get 'make 'scheme-number) n))

(define (install-scheme-number-package)
  
  (define (tag x) (attach-tag 'scheme-number x))

  ; external
  
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))

  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))

  (put '=zero? '(scheme-number) (lambda (x) (= 0 x)))
  
  (put 'make 'scheme-number (lambda (x) (tag x))))

(install-scheme-number-package)

(#%provide make-scheme-number)