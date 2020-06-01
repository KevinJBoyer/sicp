#lang sicp


(define (make-monitored f)
  (let ((times-called 0))
    (lambda (input)
      (cond ((eq? input 'how-many-calls?) times-called)
            ((eq? input 'reset-count) (set! times-called 0))
            (else (set! times-called (inc times-called)) (f input))))))

(define s (make-monitored sqrt))
(define i (make-monitored inc))

(s 100)
(s 'how-many-calls?)
(i 1)
(i 2)
(i 'reset-count)
(i 3)
(i 'how-many-calls?)