#lang sicp

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set) (cons x set))

(define (intersection-set set1 set2)
  (cond ((null? set2) '())
        ((element-of-set? (car set2) set1)
         (cons (car set2) (intersection-set set1 (cdr set2))))
        (else (intersection-set set1 (cdr set2)))))

(define (union-set set1 set2)
  (cond ((null? set2) set1)
        ((not (element-of-set? (car set2) set1))
         (cons (car set2) (union-set set1 (cdr set2))))
        (else (union-set set1 (cdr set2)))))

(union-set (list 1 2 3)
           (list 4 5 6))


(union-set (list 1 2 3)
           (list 4 5 3))


(union-set (list 1 2 3)
           (list 1 2 3))

(intersection-set (list 1 2 3)
                  (list 4 5 6))


(intersection-set (list 1 2 3)
                  (list 4 5 3))


(intersection-set (list 1 2 3)
                  (list 1 2 3))