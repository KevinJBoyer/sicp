#lang sicp


(#%require "arithmetic-system.scm")
(#%require "complex-package.scm")

(#%require "real-package.scm")

(#%require "test-func.scm")

(define complex1 (make-complex-from-real-imag 3 4))
(define complex2 (make-complex-from-mag-ang 5 (make-real (/ 3.1415926535 6))))

(test "complex make-from-real-imag"
      complex1
      (cons 'complex (cons 'rectangular (cons 3 4))))

(test "complex make-from-mag-ang"
      complex2
      (cons 'complex (cons 'polar (cons 5 (make-real (/ 3.1415926535 6))))))

(test "complex add"
      (add complex1 complex2)
      (make-complex-from-real-imag (make-real 7.330127018959607) (make-real 6.499999999935198)))

(test "complex sub"
      (sub complex1 complex2)
      (make-complex-from-real-imag (make-real -1.3301270189596073) (make-real 1.5000000000648024)))

(test "complex mul"
      (mul complex1 complex2)
      (make-complex-from-mag-ang 25 (make-real 1.5235987755833333)))

(test "complex div"
      (div complex1 complex2)
      (make-complex-from-mag-ang 1 (make-real 0.47640122441666666)))

(test "complex real-part" (real-part complex1) 3)
(test "complex imag-part" (imag-part complex1) 4)
(test "complex magnitude" (magnitude complex2) 5)
(test "complex angle" (angle complex2) (cons 'real (/ 3.1415926535 6)))

(test "complex equ?" (equ? complex1 complex2) false)
(test "complex equ?" (equ? complex2 complex2) true)
(test "complex =zero?" (=zero? complex1) false)
(test "rational =zero?" (=zero? (make-complex-from-real-imag 0 0)) true)

; tests for complex numbers made of other kinds of numbers

(#%require "integer-package.scm")
(#%require "rational-package.scm")
(#%require "real-package.scm")

(define complex-rect-rat-real1 (make-complex-from-real-imag (make-rational 3 5) (make-real 4.14159265)))
(define complex-polar-integer-rat1 (make-complex-from-mag-ang (make-integer 6) (make-rational 2 3)))
(define complex-polar-integer-rat2 (make-complex-from-mag-ang (make-integer 2) (make-rational 1 3)))

(test "complex make-from-real-imag rational real"
      complex-rect-rat-real1
      (cons 'complex (cons 'rectangular (cons (cons 'rational (cons 3 5)) (cons 'real 4.14159265)))))

(test "complex make-from-mag-ang integer rational"
      complex-polar-integer-rat1
      (cons 'complex (cons 'polar (cons 6 (cons 'rational (cons 2 3))))))

(test "complex polar int rat div"
      (div complex-polar-integer-rat1 complex-polar-integer-rat2)
      (cons 'complex (cons 'polar (cons 3 (cons 'rational (cons 1 3))))))