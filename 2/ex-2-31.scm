#lang sicp

(define (square n) (* n n))

(define (square-tree tree) (tree-map square tree))

(define (tree-map f t)
  (cond ((null? t) nil)
        ((not (pair? t)) (f t))
        (else (cons (tree-map f (car t))
                    (tree-map f (cdr t))))))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))