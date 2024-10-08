#lang sicp

(#%require "type-table.scm")
(#%require "apply-generic.scm")
(#%require "arithmetic-system.scm")

(#%require "integer-package.scm")
(#%require "real-package.scm")
(#%require "rational-package.scm")
(#%require "complex-package.scm")
(#%require "poly-package.scm")

(put 'raise '(integer)
     (lambda (int) (make-rational int 1)))

(put 'raise '(rational)
     (lambda (rat) (make-real (/ (car rat) (cdr rat)))))

(put 'raise '(real)
     (lambda (real) (make-complex-from-real-imag real 0)))

(put 'raise '(complex)
     (lambda (complex) (make-polynomial 'x (list (list 0 (cons 'complex complex))))))

(put 'project '(rational)
     (lambda (rat)
       (if (number? (car rat))
           (make-integer (round (div (car rat) (cdr rat))))
           -1
       )))

(put 'project '(real)
     (lambda (real) (make-rational (round (* 100 real)) 100)))

(put 'project '(complex)
     (lambda (complex) (make-real (real-part complex))))

(define (raise x) (apply-generic 'raise x))
(define (project x) (apply-generic 'project x))

(#%provide raise)
(#%provide project)