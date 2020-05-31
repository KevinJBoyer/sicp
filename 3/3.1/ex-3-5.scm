#lang sicp

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
          ((experiment) (iter (dec trials-remaining) (inc trials-passed)))
          (else (iter (dec trials-remaining) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral p x1 x2 y1 y2)
  (define (try-rand-point)
    (p (random-in-range x1 x2) (random-in-range y1 y2)))
  (* (* (- x2 x1) (- y2 y1))
     (monte-carlo 100000 try-rand-point)))

(define (point-in-unit-circle? x y)
  (>= (* .5 .5)
      (+ (* (- x .5) (- x .5))
         (* (- y .5) (- y .5)))))

(/ 
 (estimate-integral point-in-unit-circle? 0.0 1.0 0.0 1.0)
 (* .5 .5))