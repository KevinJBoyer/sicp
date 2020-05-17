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

(define (rotate-90 painter)
  (transform-painter
   painter
   (make-vect 0.0 1.0)
   (make-vect 0.0 0.0) ; new end of edge 1
   (make-vect 1.0 1.0) ; new end of edge 2
   ))

(define (rotate-270 painter)
  (transform-painter
   painter
   (make-vect 1.0 0.0)
   (make-vect 1.0 1.0) ; new end of edge 1
   (make-vect 0.0 0.0) ; new end of edge 2
   ))


(define (below-alt painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
           (transform-painter
            painter1
            (make-vect 0.0 0.0)
            (make-vect 1.0 0.0)
            split-point))
          (paint-top
           (transform-painter
            painter2
            split-point
            (make-vect 1.0 .5)
            (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

(define (below painter1 painter2)
  (rotate-270 (beside
               (rotate-90 painter1)
               (rotate-90 painter2))))

(paint (below einstein mark-of-zorro))