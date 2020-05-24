#lang sicp

(define (equ? x y) (apply-generic 'equ? x y))

(put 'equ '(scheme-number scheme-number)
     (lambda (x y) (= x y)))

(put 'equ '(rational rational)
     (lambda (x y) (= (* (denom x) (numer y)) (* (numer x) (denom y)))))

(put 'equ '(complex complex)
     (lambda (x y) (and (= (real x) (real y)) (= (imag x) (imag y)))))