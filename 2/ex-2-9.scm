#lang scheme

(define (abs x) (if (> x 0) x (- x)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))

(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))
(define (width i) (/ (- (cdr i) (car i)) 2))

(define ex-int-a (make-interval 1 7))
(define ex-int-b (make-interval 5 9))
(width ex-int-a)
(width ex-int-b)
(width (add-interval ex-int-a ex-int-b))
(width (mul-interval ex-int-a ex-int-b))
(newline)
(define ex-int-c (make-interval 5 11))
(define ex-int-d (make-interval -7 -3))
(width ex-int-c)
(width ex-int-d)
(width (add-interval ex-int-c ex-int-d))
(width (mul-interval ex-int-c ex-int-d))