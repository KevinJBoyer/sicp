#lang sicp

(#%require "type-table.scm")

(define (make-integer n) ((get 'make 'integer) n))

(define (install-integer-package)

  (define (make-int n) (round n))
  
  (define (tag x) (attach-tag 'integer x))

  ; external

  (put 'reduce '(integer integer)
       (lambda (n d)
         (let ((g (gcd n d)))
           (list (/ n g) (/ d g)))))
  
  (put 'add '(integer integer)
       (lambda (x y) (tag (make-int (+ x y)))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (make-int (- x y)))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (make-int (* x y)))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (make-int (/ x y)))))
  (put 'gcd '(integer integer)
       (lambda (x y) (gcd x y)))

  (put 'equ? '(integer integer)
       (lambda (x y) (= x y)))

  (put '=zero? '(integer) (lambda (x) (= 0 x)))

  (put 'sine '(integer) (lambda (x) (tag (make-int (sin x)))))
  (put 'cosine '(integer) (lambda (x) (tag (make-int (cos x)))))
  (put 'square-root '(integer) (lambda (x) (tag (make-int (sqrt x)))))
  (put 'atangen '(integer integer) (lambda (x y) (tag (make-int (atan x y)))))
  
  (put 'make 'integer (lambda (x) (tag (make-int x)))))

(install-integer-package)

(#%provide make-integer)