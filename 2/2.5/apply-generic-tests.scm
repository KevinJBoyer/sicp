#lang sicp

(#%require "type-table.scm")
(#%require "apply-generic.scm")
(#%require "arithmetic-system.scm")
(#%require "integer-package.scm")
(#%require "rational-package.scm")
(#%require "real-package.scm")
(#%require "complex-package.scm")
(#%require "coercion.scm")

(#%require "test-func.scm")

(test "appy-generic level integer"
      (level (make-integer 5)) 3)

(test "appy-generic level rational"
      (level (make-rational 2 5)) 2)

(test "appy-generic level real"
      (level (make-real 5.1)) 1)

(test "appy-generic level complex"
      (level (make-complex-from-real-imag 1 2)) 0)

(test "apply-generic add integer complex"
      (add (make-integer 2) (make-complex-from-real-imag 3 5))
      (make-complex-from-real-imag 5 5))

(test "apply-generic add real rational"
      (add (make-real 2.2) (make-rational 5 2))
      (make-real 4.7))

(test "apply-generic drop"
      (drop (make-complex-from-real-imag 3.141592 3))
      (make-complex-from-real-imag 3.141592 3))

(test "apply-generic drop"
      (drop (make-complex-from-real-imag 3.141592 0))
      (make-real 3.141592))

(test "apply-generic drop"
      (drop (make-complex-from-real-imag 3.5 0))
      (make-rational 7.0 2.0))

(test "apply-generic drop"
      (drop (make-complex-from-real-imag 3 0))
      (make-integer 3))
