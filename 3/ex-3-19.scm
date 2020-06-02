#lang sicp

(define (has-cycle? l)
  (define (safe-cdr x)
    (if (not (pair? x)) '() (cdr x)))
  
  (define (caught-up? tortoise hare)
    (cond ((null? hare) false)
          ((eq? tortoise hare) true)
          (else (caught-up?
                 (safe-cdr tortoise)
                 (safe-cdr (safe-cdr hare))))))

  (caught-up? l (safe-cdr (safe-cdr l))))


(has-cycle? (list '1 2 3 4))
(define x (list '1 2 3 4))
(set-cdr! x x)
(has-cycle? x)
        
    