#lang racket

(define (fr n)
  (if (< n 3)
      n
      (+ (fr (- n 1)) (* 2 (fr (- n 2))) (* 3 (fr (- n 3))))))

 (fr 19)

(define (f n)
  (if (< n 3)
      n
      (fi n 3 2 1 0)
  ))

(define (fic cm1 cm2 cm3) (+ cm1 (* 2 cm2) (* 3 cm3)))
  
(define (fi n count cm1 cm2 cm3)
  (if (= count n)
      (fic cm1 cm2 cm3)
      (fi n (+ count 1) (fic cm1 cm2 cm3) cm1 cm2 )))


 (f 190)