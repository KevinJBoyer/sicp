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

(test "coercion raise integer"
      (raise (make-integer 5))
      (make-rational 5 1))

(test "coercion raise rational"
      (raise (make-rational 3 7))
      (make-real (/ 300 700)))

(test "coercion raise real"
      (raise (make-real 3.14))
      (make-complex-from-real-imag 3.14 0))

(test "coercion project complex"
      (project (make-complex-from-real-imag 3.14 -2))
      (make-real 3.14))

(test "coercion project real"
      (project (make-real 3.14))
      (make-rational 157.0 50.0))

(test "coercion project rational"
      (project (make-rational 314 100))
      (make-integer 3))