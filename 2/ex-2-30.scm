#lang sicp

(define (square-tree t)
  (if (null? t)
      nil
      (if (not (pair? t))
          (* t t)
          (cons (square-tree (car t))
                (square-tree (cdr t))))))

(define (square-tree-map t)
  (map (lambda (sub-tree)
         (if (null? sub-tree)
             nil
             (if (not (pair? sub-tree))
                 (* sub-tree sub-tree)
                 (square-tree-map sub-tree))))
       t))


(square-tree-map
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))