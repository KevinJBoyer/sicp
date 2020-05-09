#lang racket


(define (smallest-divisor n)
  (define (next-divisor i)
    (if (= i 2) 3 (+ i 2)))
  (define (smallest-divisor-iter i)
    (cond ((> (* i i) n) n)
          ((= 0 (remainder n i)) i)
          (else (smallest-divisor-iter (next-divisor i)))))
  (smallest-divisor-iter 2))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)
(smallest-divisor 199999)
(smallest-divisor 1999999)
(smallest-divisor 19999999)
(smallest-divisor 199999999)