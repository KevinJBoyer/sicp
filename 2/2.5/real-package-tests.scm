#lang sicp


(#%require "arithmetic-system.scm")
(#%require "real-package.scm")

(#%require "test-func.scm")

(define real1 (make-real 9.3))
(define real2 (make-real 3.100005))

(test "real add" (add real1 real2) (make-real 12.400005))
(test "real sub" (sub real1 real2) (make-real 6.199995000000001))
(test "real mul" (mul real1 real2) (make-real 28.8300465))
(test "real div" (div real1 (make-real 6.007)) (make-real 1.5481937739304148))
(test "real equ?" (equ? real1 real2) false)
(test "real equ?" (equ? real1 (make-real 9.3)) true)
(test "real =zero?" (=zero? real1) false)
(test "real =zero?" (=zero? (make-real 0.0)) true)

(test "real sine" (sine real1) (make-real (sin 9.3)))
(test "real cosine" (cosine real1) (make-real (cos 9.3)))
(test "real square-root" (square-root real1) (make-real (sqrt 9.3)))
(test "real atangen" (atangen real1 real2) (make-real (atan 9.3 3.100005)))