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

; segments
(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))
(define (midpoint-segment s)
  (make-point (/
               (+ (x-point (start-segment s))
                  (x-point (end-segment s)))
               2)
              (/
               (+ (y-point (start-segment s))
                  (y-point (end-segment s)))
               2)))

(define p1 (make-point 2 3))
(define p2 (make-point 6 9))
(define s (make-segment p1 p2))
(define mp (midpoint-segment s))
(print-point mp)