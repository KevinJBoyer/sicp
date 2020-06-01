#lang sicp

(define (has-cycle x)
  (let ( (seen '()) )
    (define (iter struct)
      (if (null? struct)
          false
          (if (memq struct seen)
              true
              (begin
                (set! seen (cons struct seen))
                (iter (cdr struct))))))
  (iter x)))

(has-cycle '(1 2 3))
(define x '(1))
(set-cdr! x x)
(has-cycle x)