#lang sicp

(#%require "arithmetic-system.scm")

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))

(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))
(define (neg-termlist term-list)
  (if (null? term-list)
      (the-empty-termlist)
      (adjoin-term (make-term (order (first-term term-list))
                              (mul -1 (coeff (first-term term-list))))
                   (neg-termlist (rest-terms term-list)))))

(#%provide adjoin-term)
(#%provide the-empty-termlist)
(#%provide first-term)
(#%provide rest-terms)
(#%provide empty-termlist?)
(#%provide make-term)
(#%provide order)
(#%provide coeff)
(#%provide neg-termlist)