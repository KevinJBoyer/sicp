#lang sicp

(define rand
  (let ((x 5))
    (lambda (msg)
      (cond ((eq? msg 'generate) (begin (set! x (rand-update x)) x))
            ((eq? msg 'reset) (lambda (n) (set! x n)))
            (else "error")))))

(define (rand-update x)
  (remainder (+ (* 131434 x) 29533) 100))