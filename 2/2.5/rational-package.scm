#lang sicp

(#%require "type-table.scm")
(#%require "arithmetic-system.scm")

(define (make-rational n d) ((get 'make 'rational) n d))

(define (install-rational-package)
  ; internal
  (define numer car)
  (define denom cdr)
  (define (make-rat n d)
    (let ((reduced (reduce n d)))
      (cons (car reduced) (cadr reduced))))

  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))

  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))

  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))

  (define (tag x) (attach-tag 'rational x))


  ; external
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  
  (put 'equ? '(rational rational)
       (lambda (x y) (= (* (numer y) (denom x)) (* (numer x) (denom y)))))
  
  (put '=zero? '(rational) (lambda (x) (= 0 (numer x))))

  ; these need to be fixed
  (put 'sine '(rational)
       (lambda (x) (tag (make-rat
                         (* 100 (sin (/ (numer x) (denom x))))
                         100))))
  (put 'cosine '(rational)
       (lambda (x) (tag (make-rat
                         (* 100 (cos (/ (numer x) (denom x))))
                         100))))
  (put 'square-root '(rational)
       (lambda (x) (tag (make-rat
                         (* 100 (sqrt (/ (numer x) (denom x))))
                         100))))
  (put 'atangen '(rational rational)
       (lambda (x y) (tag (make-rat
                         (* 100 (atan (/ (numer x) (denom x))
                                      (/ (numer y) (denom y))))
                         100))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d)))))



(install-rational-package)

(#%provide make-rational)