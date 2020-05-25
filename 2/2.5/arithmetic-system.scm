#lang sicp

(#%require "type-table.scm")
(#%require "apply-generic.scm")

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

(#%provide add)
(#%provide sub)
(#%provide mul)
(#%provide div)
(#%provide equ?)
(#%provide =zero?)