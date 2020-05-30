#lang sicp

(#%require "arithmetic-system.scm")
(#%require "poly-package.scm")
(#%require "terms.scm")

(#%require "test-func.scm")

(define poly1 (make-polynomial 'x (make-terms-from-sparse '((3 4) (2 7) (1 1) (0 5)))))
(define poly2 (make-polynomial 'x (make-terms-from-sparse '((1 1) (0 -3)))))

(test "poly make-polynomial sparse" poly1
      '(poly x sparse-terms (3 4) (2 7) (1 1) (0 5)))

(test "poly add sparse" (add poly1 poly2)
      '(poly x sparse-terms (3 4) (2 7) (1 2) (0 2)))

(test "poly mul sparse" (mul poly2 poly1)
      '(poly x sparse-terms (4 4) (3 -5) (2 -20) (1 2) (0 -15)))

(test "poly =zero? sparse" (=zero? poly1) false)
(test "poly =zero? sparse" (=zero? (make-polynomial 'y (make-terms-from-sparse '()))) true)

(test "poly sub sparse" (sub poly1 poly1) (make-polynomial 'x (make-terms-from-sparse '())))
(test "poly sub sparse" (sub poly1 poly2) '(poly x sparse-terms (3 4) (2 7) (0 8)))

(define poly3 (make-polynomial 'z (make-terms-from-dense '(3 4 2 1))))
(define poly4 (make-polynomial 'z (make-terms-from-dense '(5 0 2))))

(test "poly make-polynomial dense" poly4
      '(poly z dense-terms 5 0 2))

(test "poly add dense" (add poly3 poly4)
      '(poly z dense-terms 3 9 2 3))


(test "poly add dense" (add poly4 poly3)
      '(poly z dense-terms 3 9 2 3))

(test "poly add dense" (add poly3 poly3)
      '(poly z dense-terms 6 8 4 2))

(define poly5 (make-polynomial 'z (make-terms-from-dense '(7 5 -3))))
(define poly6 (make-polynomial 'z (make-terms-from-dense '(8 2))))
(define poly7 (make-polynomial 'z (make-terms-from-dense '())))
(define poly8 (make-polynomial 'z (make-terms-from-dense '(56 54 -14 -6))))

(test "poly mul dense" (mul poly7 poly7) (make-polynomial 'z (make-terms-from-dense '())))
(test "poly mul dense" (mul poly6 poly7) (make-polynomial 'z (make-terms-from-dense '())))
(test "poly mul dense" (mul poly7 poly5) (make-polynomial 'z (make-terms-from-dense '())))
(test "poly mul dense" (mul poly5 poly6) poly8)
(test "poly mul dense" (mul poly6 poly5) poly8)