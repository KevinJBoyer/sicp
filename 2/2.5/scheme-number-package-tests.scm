#lang sicp


(#%require "arithmetic-system.scm")
(#%require "scheme-number-package.scm")

(#%require "test-func.scm")

(define sn1 (make-scheme-number 9))
(define sn2 (make-scheme-number 3))

(test "scheme-number add" (add sn1 sn2) (make-scheme-number 12))
(test "scheme-number sub" (sub sn1 sn2) (make-scheme-number 6))
(test "scheme-number mul" (mul sn1 sn2) (make-scheme-number 27))
(test "scheme-number div" (div sn1 sn2) (make-scheme-number 3))
(test "scheme-number equ?" (equ? sn1 sn2) false)
(test "scheme-number equ?" (equ? sn1 9) true)
(test "scheme-number =zero?" (=zero? sn1) false)
(test "scheme-number =zero?" (=zero? 0) true)