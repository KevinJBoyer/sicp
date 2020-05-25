#lang sicp


(#%require "arithmetic-system.scm")
(#%require "real-package.scm")

(#%require "test-func.scm")

(define real1 (make-real 9.3))
(define real2 (make-real 3.1))

(test "real add" (add real1 real2) (make-real 12.4))
(test "real sub" (sub real1 real2) (make-real 6.200000000000001))
(test "real mul" (mul real1 real2) (make-real 28.830000000000002))
(test "real div" (div real1 real2) (make-real 3.0))
(test "real equ?" (equ? real1 real2) false)
(test "real equ?" (equ? real1 (make-real 9.3)) true)
(test "real =zero?" (=zero? real1) false)
(test "real =zero?" (=zero? (make-real 0.0)) true)