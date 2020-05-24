#lang sicp

; tests

(#%require "type-table.scm")
(#%require "sec-2-5-1-a.scm")

(#%require "ex-2-84.scm")

(install-scheme-number-package)
(install-complex-package)
(install-rational-package)

; some sample datums to play around with

(define n1 (make-scheme-number 11))
(define n2 (make-scheme-number 19))

(define r1 (make-rational 3 5))
(define r2 (make-rational 7 4))

(define c1 (make-complex-from-real-imag 13 17))
(define c2 (make-complex-from-mag-ang 5 3.14))
(define c3 (make-complex-from-real-imag -4 2))
(define c4 (make-complex-from-real-imag 51 52))

; coercions available
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))
(put-coercion 'scheme-number 'complex scheme-number->complex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; add real numbers as a data type
(define (make-real n) (attach-tag 'real n))

; add coercions...
(define (scheme-number->rational n) (make-rational n 1))
(define (rational->real n) (make-real (/ (car n) (cdr n))))
(define (real->complex n) (make-complex-from-real-imag n 0))

(put 'raise '(scheme-number) scheme-number->rational)
(put 'raise '(rational) rational->real)
(put 'raise '(real) real->complex)

; add an op that only works on two complex numbers
(define (only-complexcc c1 c2) (add (attach-tag 'complex c1) (attach-tag 'complex c2)))
(put 'only-complex '(complex complex) only-complexcc)
(define (only-complex x y) (apply-generic 'only-complex x y))

(only-complex n1 r1)