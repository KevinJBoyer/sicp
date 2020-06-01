#lang sicp


(define (make-accumulator initial-amount)
  (let ((sum initial-amount))
    (lambda (additional-amount)
      (set! sum (+ sum additional-amount))
      sum)))

(define A (make-accumulator 5))
(define B (make-accumulator 2))
(A 10)
(A 5)
(B 4)
(B 2)