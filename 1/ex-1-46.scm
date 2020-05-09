#lang scheme

(define (inc n) (+ n 1))
(define (square n) (* n n))
(define (abs n) (if (> n 0) n (- n)))

(define (double f) (lambda (x) (f (f x ))))
;(((double (double double)) inc) 5)

(define (compose f g) (lambda (x) (f (g x))))
;((compose square inc) 6)

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))
;((repeated square 2) 5)

(define (smooth f)
  (define dx 0.01)
  (lambda (x)
    (/
     (+ (f x)
        (f (- x dx))
        (f (+ x dx)))
     3)))

(define (nf-smooth f n)
  ((repeated smooth n) f))

(define tolerance 0.00001)

(define (fixed-point-old f first-guess)
  (define (close-enough? v1 v2) (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average-damp f) (lambda (x) (/ (+ x (f x)) 2)))

(define (exp x n) (if (= n 0) 1 (* x (exp x (- n 1)))))

(define (iterative-improve improve good-enough?)
  (lambda (first-guess)
    (define (try guess)
      (if (good-enough? guess)
          guess
          (try (improve guess))))
    (try first-guess)))

(define (fixed-point f first-guess)
  (define (good-enough? guess) (< (abs (- guess (f guess))) tolerance))
  (define (improve guess) (f guess))
  ((iterative-improve improve good-enough?) first-guess))

(define (nroot x n)
  (define (dampno) (floor (/ (log (+ n 1)) (log 2))))
  (fixed-point
   ((repeated average-damp (dampno))
    (lambda (y) (/ x (exp y (- n 1)))))
   1.0))
(nroot (exp 5 234) 234)

(define (sqrt n)
  (define (improve guess) (/ (+ guess (/ n guess)) 2))
  (define (good-enough? guess) (< (abs (- (square guess) n)) 0.0001))
  ((iterative-improve improve good-enough?) n))

;(sqrt 16.0)