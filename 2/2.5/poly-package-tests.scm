#lang sicp


(#%require "arithmetic-system.scm")
(#%require "terms.scm")
(#%require "poly-package.scm")

(#%require "test-func.scm")

(define poly1 (make-polynomial 'x '((3 4) (2 7) (1 1) (0 5))))
(define poly2 (make-polynomial 'x '((1 1) (0 -3))))

(test "poly make-polynomial" poly1 (cons 'poly (cons 'x '((3 4) (2 7) (1 1) (0 5)))))

(test "poly add" (add poly1 poly2)
      (cons 'poly (cons 'x '((3 4) (2 7) (1 2) (0 2)))))

(test "poly mul" (mul poly2 poly1) '(poly x (4 4) (3 -5) (2 -20) (1 2) (0 -15)))

(test "poly =zero?" (=zero? poly1) false)
(test "poly =zero?" (=zero? (make-polynomial 'y (the-empty-termlist))) true)

(test "poly sub" (sub poly1 poly1) (make-polynomial 'x (the-empty-termlist)))
(test "poly sub" (sub poly1 poly2) '(poly x (3 4) (2 7) (0 8)))