#lang scheme

(define (double f) (lambda (x) (f (f x ))))

(define (inc n) (+ n 1))

(((double (double double)) inc) 5)

(define (compose f g) (lambda (x) (f (g x))))

(define (square n) (* n n))

((compose square inc) 6)