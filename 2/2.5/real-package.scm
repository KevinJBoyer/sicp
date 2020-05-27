#lang sicp

(#%require "type-table.scm")

(define (make-real n) ((get 'make 'real) n))

(define (install-real-package)

  (define (tag x) (attach-tag 'real x))

  ; external
  
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))

  (put 'equ? '(real real)
       (lambda (x y) (= x y)))

  (put '=zero? '(real) (lambda (x) (= 0 x)))

  (put 'sine '(real) (lambda (x) (tag (sin x))))
  (put 'cosine '(real) (lambda (x) (tag (cos x))))
  (put 'square-root '(real) (lambda (x) (tag (sqrt x))))
  (put 'atangen '(real real) (lambda (x y) (tag (atan x y))))
  
  (put 'make 'real (lambda (x) (tag x))))

(install-real-package)

(#%provide make-real)