#lang sicp

(#%require "type-table.scm")
(#%require "apply-generic.scm")

(define (add-terms L1 L2) (apply-generic 'add-terms L1 L2))
(define (mul-terms L1 L2) (apply-generic 'mul-terms L1 L2))
(define (neg-termlist L) (apply-generic 'neg-termlist L))
(define (empty-termlist? L) (apply-generic 'empty-termlist? L))

(define (make-terms-from-sparse L) ((get 'make 'sparse-terms) L))
(define (make-terms-from-dense L) ((get 'make 'dense-terms) L))

(#%provide add-terms)
(#%provide mul-terms)
(#%provide neg-termlist)
(#%provide empty-termlist?)
(#%provide make-terms-from-sparse)
(#%provide make-terms-from-dense)