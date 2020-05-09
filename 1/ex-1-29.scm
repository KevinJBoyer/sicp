#lang scheme

(define (even? n) (= 0 (remainder n 2)))

(define (sum term a next b) (sum-i term a next b))

(define (sum-i term a next b)
  (define (iter a result)
  (if (= a b)
      result
      (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (sum-r term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpsons-rule f a b n)
  (define h (/ (- b a) n))
  (define (term k)
    (* (f (+ a (* k h)))
       (cond ((= 0 k) 1)
             ((even? k) 2)
             (else 4))))
  (define (next k) (+ k 1))
  (* (/ h 3.0)
     (sum term 0 next n)))

(define (cube n) (* n n n))

(simpsons-rule cube 0 1 100)
(simpsons-rule cube 0 1 1000)