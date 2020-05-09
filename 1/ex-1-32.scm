#lang scheme

(define (even? n) (= 0 (remainder n 2)))
(define (cube n) (* n n n))

(define (next-lowest-even n) (if (even? n) n (- n 1)))
(define (next-highest-odd n) (if (even? n) (+ n 1) n))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term  (next a) next b))))

(define (accumulate-i combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))


(define (prod term a next b) (accumulate-i * 1.0 term a next b))

(define (pi-approx n)
  (define (next a) (+ a 1))
  (define (nterm a) (+ 2 (next-lowest-even a)))
  (define (dterm a) (if (even? a) (+ a 1) (+ a 2)))
  (/ (prod nterm 1 next n)
     (prod dterm 1 next n)))

 (* 4 (pi-approx 100))