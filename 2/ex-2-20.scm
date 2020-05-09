#lang sicp

(define (same-parity x . l)

  (define (same-parity? n)
    (=
     (remainder x 2)
     (remainder n 2)))
    
  (define (same-parity-iter result lyst)
    (if (null? lyst)
        result
        (if (same-parity? (car lyst))
            (same-parity-iter (append result (list (car lyst))) (cdr lyst))
            (same-parity-iter result (cdr lyst)))))
  (same-parity-iter (list x) l))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)