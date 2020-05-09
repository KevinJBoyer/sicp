#lang scheme

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

(define (slow-prime? n)
  (if (= n (smallest-divisor n)) true false))

(define (smallest-divisor n)
  (define (next-divisor i)
    ;(+ i 1))
    (if (= i 2) 3 (+ i 2)))
  (define (smallest-divisor-iter i)
    (cond ((> (* i i) n) n)
          ((= 0 (remainder n i)) i)
          (else (smallest-divisor-iter (next-divisor i)))))
  (smallest-divisor-iter 2))


(define (timed-prime-test n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))
      false))
(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))
(define (runtime) (current-inexact-milliseconds))

;(timed-prime-test 87178291199)
;(timed-prime-test 263130836933693530167218012159999999)

(define (search-for-primes start count)
  (define (next-search n)
    (if (= 0 (remainder n 2)) (+ n 1) (+ n 2)))
  (cond ((= count 0) (newline))
        ((not (timed-prime-test start)) (search-for-primes (next-search start) count))
        (else (search-for-primes (next-search start) (- count 1)))))

(search-for-primes 1000 3)
(search-for-primes 10000 3)
(search-for-primes 100000 3)
(search-for-primes 100000000 3)