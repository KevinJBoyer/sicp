#lang scheme

(define (abs x) (if (> x 0) x (- x)))

(define (exp b n) (if (= n 0) 1 (* b (exp b (- n 1)))))

(define (cons x y)
  (* (exp 2 x) (exp 3 y)))

(define (car z)
  (define z2 (/ z (exp 3 (cdr z))))
  (define (extract z2 n)
    (if (= 1 (remainder z2 2))
        n
        (extract (/ z2 2) (+ n 1))))
  (extract z2 0))

(define (cdr z)
  (define (extract z n)
    (if (= 0 (remainder z 3))
        (extract (/ z 3) (+ n 1))
        n))
  (extract z 0))

(define test-pair (cons 5 9))
(car test-pair)
(cdr test-pair)