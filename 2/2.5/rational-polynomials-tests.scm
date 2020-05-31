#lang sicp

(#%require "arithmetic-system.scm")
(#%require "rational-package.scm")
(#%require "poly-package.scm")

(#%require "test-func.scm")

;(define p1 (make-polynomial 'x '(sparse-terms (1 1) (0 1))))
;(define p2 (make-polynomial 'x '(sparse-terms (3 1) (0 -1))))
;(define p3 (make-polynomial 'x '(sparse-terms (1 1))))
;(define p4 (make-polynomial 'x '(sparse-terms (2 1) (0 -1))))
;(define rf1 (make-rational p1 p2))
;(define rf2 (make-rational p3 p4))
;(add rf1 rf2)

(define a1 (make-polynomial 'x '(sparse-terms (2 1) (0 1))))
(define a2 (make-polynomial 'x '(sparse-terms (3 1) (0 1))))
(define b (make-rational a2 a1))
(add b b)