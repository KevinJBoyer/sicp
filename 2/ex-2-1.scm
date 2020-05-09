#lang scheme

(define (abs x) (if (> x 0) x (- x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y)) (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y)) (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))(* (numer y) (denom x))))

;(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (make-rat n d)
  (if (or (and (< n 0) (> d 0)) (and (> n 0) (< d 0)))
      (cons (- (abs n)) (abs d))
      (cons (abs n) (abs d))))

(print-rat (make-rat 3 5))
(print-rat (make-rat -3 5))
(print-rat (make-rat 3 -5))
(print-rat (make-rat -3 -5))
