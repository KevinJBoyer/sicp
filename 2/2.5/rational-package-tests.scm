#lang sicp


(#%require "arithmetic-system.scm")
(#%require "rational-package.scm")

(#%require "test-func.scm")

(define rat1 (make-rational 2 5))
(define rat2 (make-rational 3 7))
(test "rational gcd" (make-rational 9 3) (make-rational 3 1))
(test "rational add" (add rat1 rat2) (make-rational 29 35))
(test "rational sub" (sub rat1 rat2) (make-rational -1 35))
(test "rational mul" (mul rat1 rat2) (make-rational 6 35))
(test "rational div" (div rat1 rat2) (make-rational 14 15))
(test "rational equ?" (equ? rat1 rat2) false)
(test "rational equ?" (equ? rat1 (make-rational 4 10)) true)
(test "rational =zero?" (=zero? rat1) false)
(test "rational =zero?" (=zero? (make-rational 0 5)) true)