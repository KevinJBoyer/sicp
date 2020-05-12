#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate
             op
             init
             (accumulate (lambda (x y) (cons (car x) y)) nil seqs))
            (accumulate-n
             op
             init
             (accumulate (lambda (x y) (cons (cdr x) y)) nil seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map
   (lambda (w) (dot-product v w))
   m))

(define (transpose m)
  (accumulate-n cons nil m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map
     (lambda (v) (map (lambda (w) (dot-product v w)) cols))
     m)))

(define m (list (list 1 2 3)
                (list 4 5 6)
                (list 7 8 9)
                (list 10 11 12)))
(define n (list (list 1 2 3 4)
                (list 5 6 7 8)
                (list 9 10 11 12)))
    
(define v (list 1 2 3))
(define w (list 4 5 6))

;(dot-product v w)
;(matrix-*-vector m v)
;(transpose n)
(matrix-*-matrix m n)