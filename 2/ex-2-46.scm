#lang sicp
(#%require sicp-pict)

(define (make-vect x y) (cons x y) )

(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (comb-vect v1 v2 op)
  (make-vect (op (xcor-vect v1)
                 (xcor-vect v2))
             (op (ycor-vect v1)
                 (ycor-vect v2))))

(define (add-vect v1 v2) (comb-vect v1 v2 +))
(define (sub-vect v1 v2) (comb-vect v1 v2 -))
(define (scale-vect v s) (comb-vect v (make-vect s s) *))