#lang scheme

(define tolerance 0.00001)

(define (abs n)
  (cond ((> n 0) n)
        (else (- 0 n))))

(define (fixed-point f first-guess)
  (define (close-enough? n1 n2)
    (> tolerance (abs (- n1 n2))))
  (let ((next (f first-guess)))
    (display "x=")
    (display first-guess)
    (display " and f(x)=")
    (display next)
    (newline)
    (if (close-enough? first-guess next)
        next
        (fixed-point f next))))

;(fixed-point cos 1.0)

(define (sqrt n)
  (fixed-point (lambda (y) (/ (+ y (/ n y)) 2))
               1.0))

;(sqrt 16)
;(fixed-point (lambda (n) (+ 1 (/ 1 n)))
;             1.0)

;(fixed-point (lambda (n) (/ (log 1000) (log n))) 2)
; 4.555...

  (fixed-point (lambda (n)
               (/ (+ n (/ (log 1000) (log n)))
                  2))
             2.0)