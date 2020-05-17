#lang sicp
(#%require sicp-pict)

(define (flip-vert painter)
  (transform-painter
   painter
   (make-vect 0.0 1.0)
   (make-vect 1.0 1.0) ; new end of edge 1
   (make-vect 0.0 0.0) ; new end of edge 2
   ))

(define (flip-horiz painter)
  (transform-painter
   painter
   (make-vect 1.0 0.0)
   (make-vect 0.0 0.0) ; new end of edge 1
   (make-vect 1.0 1.0) ; new end of edge 2
   ))

(define (rotate-180 painter)
  (flip-vert (flip-horiz painter)))

(define (rotate-270 painter)
  (transform-painter
   painter
   (make-vect 1.0 0.0)
   (make-vect 1.0 1.0) ; new end of edge 1
   (make-vect 0.0 0.0) ; new end of edge 2
   ))


(paint einstein)
(paint (rotate-270 einstein))