#lang sicp

(#%require "test-func.scm")

(#%require "terms.scm")

(test "terms the-empty-termlist" (the-empty-termlist) '())
(test "terms make-term" (make-term 5 6) (list 5 6))
(test "terms adjoin-term"
      (adjoin-term (make-term 6 0)
                   (adjoin-term (make-term 2 5)
                                (the-empty-termlist)))
      (list '(2 5)))

(test "terms first-term" (first-term '((5 6) (3 4) (2 3))) '(5 6))
(test "terms rest-terms" (rest-terms '((5 6) (3 4) (2 3))) '((3 4) (2 3)))
(test "terms empty-termlist?" (empty-termlist? (the-empty-termlist)) true)
(test "terms empty-termlist?" (empty-termlist? (list '(5 6))) false)
(test "terms order" (order (make-term 5 7)) 5)
(test "terms coeff" (coeff (make-term 5 7)) 7)
(test "terms neg-termlist"
      (neg-termlist '((5 6) (2 -3) (0 6)))
      '((5 -6) (2 3) (0 -6)))