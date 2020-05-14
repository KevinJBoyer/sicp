#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval start n)
  (define (iter i)
    (if (> i n)
        nil
        (cons i (iter (+ i 1)))))
  (iter start))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

; given n, find all (i, j) such that 1 <= j < i <= n

(define (square n) (* n n))

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

(define (prime? n) (fast-prime? n 10))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
         ((predicate (car sequence))
          (cons (car sequence) (filter predicate (cdr sequence))))
         (else (filter predicate (cdr sequence)))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map
      (lambda (j) (list i j))
      (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(prime-sum-pairs 5)