#lang sicp


(define (reverse x)
  (if (null? (cdr x))
      (list (car x))
      (append (reverse (cdr x)) (list (car x)))))

(define (deep-reverse x)
  (cond ((not (pair? x)) (list x))
        ((null? (cdr x)) (deep-reverse (car x)))
        ((not (pair? (car x)))
         (append (deep-reverse (cdr x))
                 (list (car x))))
        (else
         (append (list (deep-reverse (cdr x)))
                 (list (deep-reverse (car x)))))))

(define x (list (list 1 2) (list 3 4)))
(reverse x)
(deep-reverse x)

(define y (list (list -1 0) (list (list .3 .6) 2) (list 3 4)))
(deep-reverse y)