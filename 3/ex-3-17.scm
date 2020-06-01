#lang sicp

(define (count-pairs x)
  (let ( (seen '()) )
    (define (iter struct)
      (if (or (not (pair? struct)) (memq struct seen))
          0
          (begin
            (set! seen (cons struct seen))
            (+ 1
               (iter (car struct))
               (iter (cdr struct))))))
  (iter x)))

(count-pairs (cons 3 4))
(count-pairs '(1 2 3))
(count-pairs '((1 . 2) (3 . 4) (5 . 6)))
(define x '(1))
(count-pairs (cons x x)) ; should be 2