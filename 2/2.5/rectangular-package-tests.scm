#lang sicp

(#%require "type-table.scm")
(#%require "apply-generic.scm")
(#%require "rectangular-package.scm")
(#%require "real-package.scm")

(#%require "test-func.scm")

;rectangular-package doesn't provide these directly
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (make-from-real-imag x y) ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a) ((get 'make-from-mag-ang 'rectangular) r a))

(define rect1 (make-from-real-imag 7 17))
(define rect2 (make-from-mag-ang 8 (make-real (/ 3.141592 8))))

(test "rectangular make-from-real-imag"
      rect1
      (cons 'rectangular (cons 7 17)))

(test "rectangular make-from-mag-ang"
      rect2
      (cons 'rectangular (cons (cons 'real 7.391036510208255) (cons 'real 3.0614668550824753))))

(test "rectangular real-part" (real-part rect1) 7)
(test "rectangular imag-part" (imag-part rect1) 17)
(test "rectangular magnitude" (magnitude rect2) 8.0)
(test "rectangular angle" (angle rect2) (cons 'real 0.39269899999999996))