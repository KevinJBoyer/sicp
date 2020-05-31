#lang sicp

(#%require "arithmetic-system.scm")
(#%require "rational-package.scm")
(#%require "poly-package.scm")

(#%require "test-func.scm")

;(define p1 (make-polynomial 'x '(sparse-terms (2 1) (0 1))))
;(define p2 (make-polynomial 'x '(sparse-terms (3 1) (0 1))))
;(div '(poly x sparse-terms (1 -1) (0 1))
;     '(poly x sparse-terms (0 2)))
;(define rf (make-rational p2 p1))
;(add rf rf)


(define p1 (make-polynomial 'x '(sparse-terms (2 1) (1 -2) (0 1))))
(define p2 (make-polynomial 'x '(sparse-terms (2 11) (0 7))))
(define p3 (make-polynomial 'x '(sparse-terms (1 13) (0 5))))

(define q1 (mul p1 p2))
(define q2 (mul p1 p3))
q1
q2
(gcd q1 q2)