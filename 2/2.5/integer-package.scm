#lang sicp

(#%require "type-table.scm")

(define (make-integer n) ((get 'make 'integer) n))

(define (install-integer-package)
  
  (define (tag x) (attach-tag 'integer x))

  ; external
  
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (/ x y))))

  (put 'equ? '(integer integer)
       (lambda (x y) (= x y)))

  (put '=zero? '(integer) (lambda (x) (= 0 x)))

  (put 'sine '(integer) (lambda (x) (tag (sin x))))
  (put 'cosine '(integer) (lambda (x) (tag (cos x))))
  (put 'square-root '(integer) (lambda (x) (tag (sqrt x))))
  (put 'atangen '(integer integer) (lambda (x y) (tag (atan x y))))
  
  (put 'make 'integer (lambda (x) (tag x))))

(install-integer-package)

(#%provide make-integer)