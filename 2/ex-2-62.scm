#lang sicp

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((< (car set1) (car set2))
         (cons (car set1) (union-set (cdr set1) set2)))
        ((> (car set1) (car set2))
         (cons (car set2) (union-set set1 (cdr set2))))
        ((= (car set1) (car set2))
         (cons (car set1) (union-set (cdr set1) (cdr set2))))))

(union-set (list 1 3 5 7 9)
           (list 1 3 5 7 11))

(union-set (list 1 3 5 7 9)
           (list 2 4 6 8 10))

(union-set (list 1)
           (list 2))
(union-set (list 2)
           (list 1))