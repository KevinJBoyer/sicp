#lang sicp

(define (for-each f l)
  (cond ((null? l) true)
        (else (f (car l))
              (for-each f (cdr l)))))

(for-each (lambda (x) (newline) (display x)) (list 57 321 88))