#lang scheme

(define (abs x) (if (> x 0) x (- x)))

(define (last-pair l)
  (if (null? (cdr l))
      (car l)
      (last-pair (cdr l))))

(define (reverse l)
  (if (null? (cdr l))
      (list (car l))
      (append (reverse (cdr l))
              (list (car l)))))

(reverse (list 25 16 9 4 1))