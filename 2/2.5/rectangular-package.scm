#lang sicp

(#%require "type-table.scm")

(define (install-rectangular-package)
  (define real-part car)
  (define imag-part cdr)
  (define make-from-real-imag cons)
  (define (magnitude z)
    (sqrt (+ (* (real-part z) (real-part z))
             (* (imag-part z) (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

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