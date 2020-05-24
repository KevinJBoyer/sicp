#lang sicp

; tests

(#%require "type-table.scm")
(#%require "sec-2-5-1.scm")

(#%require "ex-2-82.scm")

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

; to test we need a three-argument generic function
; we'll say this function f(a, b, c) = a + b + c for complex numbers
(define (add-three-complex a b c)
  (add (add (attach-tag 'complex a) (attach-tag 'complex b)) (attach-tag 'complex c)))
(put 'add-three '(complex complex complex) add-three-complex)
(define (add-three a b c) (apply-generic 'add-three a b c))

(add-three n1 n2 c3)