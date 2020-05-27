#lang sicp


(#%require "arithmetic-system.scm")
(#%require "integer-package.scm")

(#%require "test-func.scm")

(define int1 (make-integer 9))
(define int2 (make-integer 3))

(test "integer add" (add int1 int2) (make-integer 12))
(test "integer sub" (sub int1 int2) (make-integer 6))
(test "integer mul" (mul int1 int2) (make-integer 27))
(test "integer div" (div int1 int2) (make-integer 3))
(test "integer equ?" (equ? int1 int2) false)
(test "integer equ?" (equ? int1 9) true)
(test "integer =zero?" (=zero? int1) false)
(test "integer =zero?" (=zero? 0) true)

(test "rational sine" (sine int1) (make-integer 0.4121184852417566))
(test "rational cosine" (cosine int1) (make-integer -0.9111302618846769))
(test "rational square-root" (square-root int1) 3)
(test "rational atangen" (atangen int1 int2) (make-integer (atan 9 3)))