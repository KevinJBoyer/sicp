#lang scheme

(define (even? n) (= 0 (remainder n 2)))
(define (cube n) (* n n n))
(define (square n) (* n n))

(define (prime? n) (fast-prime? n 10))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))


(define (filtered-accumulate filter? combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (if (filter? a) (term a) null-value)
                (filtered-accumulate filter? combiner null-value term  (next a) next b))))

(define (filtered-prod filter? term a next b) (filtered-accumulate filter? * 1.0 term a next b))
(define (filtered-sum filter? term a next b) (filtered-accumulate filter? + 0.0 term a next b))


(define (inc n) (+ 1 n))
(filtered-sum prime? square 2 inc 10)

 (define (gcd m n) 
   (cond ((< m n) (gcd n m)) 
         ((= n 0) m) 
         (else (gcd n (remainder m n)))))

(define (relatively-prime? i)
  (= 1 (gcd i 10)))

(define (id n) n)
(filtered-prod relatively-prime? id 2 inc 10)