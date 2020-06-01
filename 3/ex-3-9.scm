#lang sicp

(define (dec-x)
  (define x 500)
  (lambda () (define x (dec x)) x))