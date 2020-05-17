#lang sicp

(define (equal? x y)
  (if (and (pair? x) (pair? y))
      (and (eq? (car x) (car y))
           (equal? (cdr x) (cdr y)))
       (if (and (not (pair? x)) (not (pair? y)))
           (eq? x y)
           false)))

(equal? 'a 'b) ;#f
(equal? 'a 'a) ;#t
(equal? (list 'a 'b)
        (list 'a 'a)) ;#f
(equal? (list 'a 'b)
        (list 'a 'b)) ;#t
(equal? (list 'a 'b)
        (list 'a 'b 'c)) ;#f