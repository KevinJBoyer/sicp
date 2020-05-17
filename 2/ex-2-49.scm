#lang sicp
(#%require sicp-pict)

(define outline
  (segments->painter
   (list (make-segment (make-vect 0 0) (make-vect 1 0))
         (make-segment (make-vect 0 0) (make-vect 0 1))
         (make-segment (make-vect 1 0) (make-vect 1 1))
         (make-segment (make-vect 0 1) (make-vect 1 1)))))

(paint outline)

(define cross
  (segments->painter
   (list (make-segment (make-vect 0 0) (make-vect 1 1))
         (make-segment (make-vect 0 1) (make-vect 1 0)))))

(paint cross)

(define diamond
  (segments->painter
   (list (make-segment (make-vect .5 0) (make-vect 0 .5))
         (make-segment (make-vect .5 1) (make-vect 1 .5))
         (make-segment (make-vect 0 .5) (make-vect .5 1))
         (make-segment (make-vect .5 0) (make-vect 1 .5)))))

(paint diamond)