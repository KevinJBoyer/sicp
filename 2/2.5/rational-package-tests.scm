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

(test "rational sine" (sine rat1) (make-rational 1370143985400371.0 3.5184372088832e+15))
(test "rational cosine" (cosine rat1) (make-rational 3240695272950697.0 3.5184372088832e+15))
(test "rational square-root" (square-root rat1) (make-rational 8901020307485223.0 1.40737488355328e+16))
(test "rational atangen" (atangen rat1 rat2) (make-rational (atan (/ 2 5) (/ 3 7)) 1))