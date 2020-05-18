#lang sicp

(define (union-set set1 set2)
  (cond ((null? set2) set1)
        ((not (memq (car set2) set1))
         (cons (car set2) (union-set set1 (cdr set2))))
        (else (union-set set1 (cdr set2)))))

(union-set (list 1 2 3)
           (list 4 5 6))


(union-set (list 1 2 3)
           (list 4 5 3))


(union-set (list 1 2 3)
           (list 1 2 3))