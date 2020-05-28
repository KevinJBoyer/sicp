#lang sicp

(#%require "type-table.scm")
(#%require "apply-generic.scm")
(#%require "polar-package.scm")

(#%require "real-package.scm")

(#%require "test-func.scm")

;polar-package doesn't provide these directly
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (make-from-real-imag x y) ((get 'make-from-real-imag 'polar) x y))
(define (make-from-mag-ang r a) ((get 'make-from-mag-ang 'polar) r a))

(define polar1 (make-from-real-imag 3 (make-real 4)))
(define polar2 (make-from-mag-ang 8 (make-real (/ 3.141592 8))))

(test "polar make-from-real-imag"
      polar1
      (cons 'polar (cons 5 (cons 'real 0.9272952180016122))))

(test "polar make-from-mag-ang"
      polar2
      (cons 'polar (cons 8 (cons 'real (/ 3.141592 8)))))

(test "polar real-part" (real-part polar2) (cons 'real 7.391036510208255))
(test "polar imag-part" (imag-part polar2) (cons 'real 3.0614668550824753))
(test "polar magnitude" (magnitude polar1) 5)
(test "polar angle" (angle polar2) (cons 'real .392699))