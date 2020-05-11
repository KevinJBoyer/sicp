#lang sicp


(define (fringe x)
  (if (not (pair? x))
        (list x)
        (if (null? (cdr x))
            (fringe (car x))
            (append (fringe (car x)) (fringe (cdr x))))))


(define x (list (list 1 2) (list 3 4)))
(fringe x)
(fringe (list x x))