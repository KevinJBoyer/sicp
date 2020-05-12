#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (horner-eval x seq)
  (define (horner asubnminus1 asubn)
    (+ asubnminus1 (* x asubn)))
  (accumulate horner 0 seq))

(horner-eval 2 (list 1 3 0 5 0 1))