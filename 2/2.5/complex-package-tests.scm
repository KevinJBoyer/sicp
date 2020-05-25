#lang sicp


(#%require "arithmetic-system.scm")
(#%require "complex-package.scm")

(#%require "test-func.scm")

(define complex1 (make-from-real-imag 3 4))
(define complex2 (make-from-mag-ang 5 (/ 3.1415926535 6)))

(test "complex make-from-real-imag"
      complex1
      (cons 'complex (cons 'rectangular (cons 3 4))))

(test "complex make-from-mag-ang"
      complex2
      (cons 'complex (cons 'polar (cons 5 (/ 3.1415926535 6)))))

(test "complex add"
      (add complex1 complex2)
      (make-from-real-imag 7.330127018959607 6.499999999935198))

(test "complex sub"
      (sub complex1 complex2)
      (make-from-real-imag -1.3301270189596073 1.5000000000648024))

(test "complex mul"
      (mul complex1 complex2)
      (make-from-mag-ang 25 1.4508939935849456))

(test "complex div"
      (div complex1 complex2)
      (make-from-mag-ang 1 0.40369644241827884))

(test "complex real-part" (real-part complex1) 3)
(test "complex imag-part" (imag-part complex1) 4)
(test "complex magnitude" (magnitude complex2) 5)
(test "complex angle" (angle complex2) (/ 3.1415926535 6))

(test "complex equ?" (equ? complex1 complex2) false)
(test "complex equ?" (equ? complex2 complex2) true)
(test "complex =zero?" (=zero? complex1) false)
(test "rational =zero?" (=zero? (make-from-real-imag 0 0)) true)