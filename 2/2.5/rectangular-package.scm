#lang sicp

(#%require "type-table.scm")
(#%require "arithmetic-system.scm")

(define (install-rectangular-package)
  (define real-part car)
  (define imag-part cdr)
  (define make-from-real-imag cons)
  (define (magnitude z)
    (square-root (add (mul (real-part z) (real-part z))
               (mul (imag-part z) (imag-part z)))))
  (define (angle z)
    (atangen (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (mul r (cosine a)) (mul r (sine a))))

  (define (tag x) (attach-tag 'rectangular x))

  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a)))))

(install-rectangular-package)