#lang scheme

(define (abs x) (if (> x 0) x (- x)))

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(define test-pair (cons 5 9))
(car test-pair)
(cdr test-pair)