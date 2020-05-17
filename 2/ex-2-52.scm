#lang sicp
(#%require sicp-pict)

(define (up-split painter n)
 (if (= n 0)
  painter 
  (let ((smaller (up-split painter (- n 1))))
   (below painter (beside smaller smaller)))))

(define (right-split painter n)
 (if (= n 0)
  painter
  (let ((smaller (right-split painter (- n 1))))
   (beside painter (below smaller smaller)))))

(define (corner-split painter n)
 (if (= n 0)
  painter
  (let ((up (up-split painter (- n 1)))
        (right (right-split painter (- n 1))))
   (let ((top-left (beside up up))
         (bottom-right right)
         (corner (corner-split painter (- n 1))))
    (beside (below painter top-left)
            (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below half (flip-vert half)))))

(paint (square-limit

       (segments->painter (list (make-segment (make-vect 0 0) (make-vect 1 1))))
        
        3))