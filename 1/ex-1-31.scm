#lang scheme

(define (even? n) (= 0 (remainder n 2)))
(define (cube n) (* n n n))

(define (next-lowest-even n) (if (even? n) n (- n 1)))
(define (next-highest-odd n) (if (even? n) (+ n 1) n))

(define (prod term a next b) (prod-i term a next b))

(define (prod-i term a next b)
  (define (iter a result)
  (if (> a b)
      result
      (iter (next a) (* (term a) result))))
  (iter a 1.0))

(define (prod-r term a next b)
  (if (> a b)
      1.0
      (* (term a)
         (prod-r term (next a) next b))))

(define (pi-approx n)
  (define (next a) (+ a 1))
  (define (nterm a) (+ 2 (next-lowest-even a)))
  (define (dterm a) (if (even? a) (+ a 1) (+ a 2)))
  (/ (prod nterm 1 next n)
     (prod dterm 1 next n)))

 (* 4 (pi-approx 100))