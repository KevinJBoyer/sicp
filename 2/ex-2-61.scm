#lang sicp

(define (adjoin-set x set)
  (define (adjoin-r x before after)
    (cond ((null? after) (append before (list x)))
          ((= x (car after)) (append before after))
          ((< x (car after)) (append before (list x) after))
          (else (adjoin-r x
                          (append before (list (car after)))
                          (cdr after)))))
  (adjoin-r x nil set))

(adjoin-set 5 (list 1 2 3))
(adjoin-set 5 (list 1 2 3 6 7))
(adjoin-set 5 (list 1 2 3 5 6 7))
(adjoin-set 5 (list 1 2 3 5))
(adjoin-set 5 (list 5 6))