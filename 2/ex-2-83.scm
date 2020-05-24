#lang sicp

(#%require "type-table.scm") ; types
(#%require "sec-2-5-1.scm")  ; scheme number, complex, and rational packages
(#%require "ex-2-82.scm")    ; apply-generic

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

; add real numbers as a data type
(define (make-real n) (attach-tag 'real n))

; add coercions...
(define (scheme-number->rational n) (make-rational n 1))
(define (rational->real n) (make-real (/ (car n) (cdr n))))
(define (real->complex n) (make-complex-from-real-imag n 0))

(put 'raise '(scheme-number) scheme-number->rational)
(put 'raise '(rational) rational->real)
(put 'raise '(real) real->complex)

(define (raise x) (apply-generic 'raise x))
