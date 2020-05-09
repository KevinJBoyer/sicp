#lang scheme

(define (inc n) (+ n 1))
(define (square n) (* n n))

(define (double f) (lambda (x) (f (f x ))))
;(((double (double double)) inc) 5)

(define (compose f g) (lambda (x) (f (g x))))
;((compose square inc) 6)

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))
;((repeated square 2) 5)

(define (smooth f)
  (define dx 0.01)
  (lambda (x)
    (/
     (+ (f x)
        (f (- x dx))
        (f (+ x dx)))
     3)))

(define (nf-smooth f n)
  ((repeated smooth n) f))