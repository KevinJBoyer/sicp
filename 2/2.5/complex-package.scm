#lang sicp

(#%require "type-table.scm")
(#%require "apply-generic.scm")

(#%require "rectangular-package.scm")
(#%require "polar-package.scm")

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (install-complex-package)
  ; internal
  
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))

  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  
  (define (tag x) (attach-tag 'complex x))

  ; external
  
  (put 'add '(complex complex)
       (lambda (x y) (tag (add-complex x y))))
  (put 'sub '(complex complex)
       (lambda (x y) (tag (sub-complex x y))))
  (put 'mul '(complex complex)
       (lambda (x y) (tag (mul-complex x y))))
  (put 'div '(complex complex)
       (lambda (x y) (tag (div-complex x y))))

  (put 'equ? '(complex complex) (lambda (z1 z2)
                            (and (= (real-part z1) (real-part z2))
                                 (= (imag-part z1) (imag-part z2)))))
  
  (put '=zero? '(complex) (lambda (z)
                            (and (= 0 (real-part z))
                                 (= 0 (imag-part z)))))

  
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a)))))

(install-complex-package)

(#%provide make-complex-from-real-imag)
(#%provide make-complex-from-mag-ang)

(#%provide real-part)
(#%provide imag-part)
(#%provide magnitude)
(#%provide angle)