#lang sicp

(define (=zero? x) (apply-generic '=zero? x))

(put '=zero? '(scheme-number)
     (lambda (x) (= 0 x)))

(put '=zero? '(rational)
     (lambda (x) (= 0 (numer x))))

(put '=zero? '(complex)
     (lambda (x) (= 0 (magnitude x))))