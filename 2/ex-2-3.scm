#lang scheme

(define (abs x) (if (> x 0) x (- x)))

; points
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; representation A: diagonal corners
(define (make-rectA p1 p2) (cons p1 p2))
(define (perimeter-rectA r)
  (+ (* 2 (abs (- (x-point (car r)) (x-point (cdr r)))))
     (* 2 (abs (- (y-point (car r)) (y-point (cdr r)))))))
(define (area-rectA r)
  (* (abs (- (x-point (car r)) (x-point (cdr r))))
     (abs (- (y-point (car r)) (y-point (cdr r))))))

(define rectA (make-rectA (make-point 2 1) (make-point 5 9)))
(display (perimeter-rectA rectA))
(newline)
(display (area-rectA rectA))
(newline)

; representation B: bottom left point + width + height
(define (make-rectB p w h) (cons p (cons w h)))
(define (perimeter-rectB r)
  (+ (* 2 (car (cdr r)))
     (* 2 (cdr (cdr r)))))
(define (area-rectB r)
  (* (car (cdr r))  ;width
     (cdr (cdr r)))) ;height

(define rectB (make-rectB (make-point 2 1) 3 8))
(display (perimeter-rectB rectB))
(newline)
(display (area-rectB rectB))