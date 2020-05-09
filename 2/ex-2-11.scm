#lang scheme

(define (abs x) (if (> x 0) x (- x)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (define (pos? i)
    (and (>= (lower-bound i) 0)
         (>= (upper-bound i) 0)))
  (define (neg? i)
    (and (< (lower-bound i) 0)
         (< (upper-bound i) 0)))
  (define (mix? i)
    (not (or (pos? i) (neg? i))))
  
  (let ((a (lower-bound x))
        (b (upper-bound x))
        (c (lower-bound y))
        (d (upper-bound y)))
    (cond ((and (pos? x) (pos? y)) (make-interval (* a c) (* b d)))
          ((and (pos? x) (mix? y)) (make-interval (* b c) (* b d)))
          ((and (pos? x) (neg? y)) (make-interval (* b c) (* a d)))
          ((and (mix? x) (pos? y)) (make-interval (* a d) (* b d)))
          ((and (mix? x) (neg? y)) (make-interval (* b c) (* a c)))
          ((and (neg? x) (pos? y)) (make-interval (* a d) (* b c)))
          ((and (neg? x) (mix? y)) (make-interval (* a d) (* a c)))
          ((and (neg? x) (neg? y)) (make-interval (* b d) (* a c)))
          (else (make-interval
                 (min (* b c) (* a d))
                 (max (* a c) (* b d)))))))

(define (div-interval x y)
  (define (crosses-zero? i)
    (and (<= (lower-bound i) 0)
         (>= (upper-bound i) 0)))

  (if (or (crosses-zero? x)
          (crosses-zero? y))
      (display "error")
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))

(define (make-interval a b) (cons a b))
(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))
(define (width i) (/ (- (cdr i) (car i)) 2))

(define posix (make-interval 3 7))
(define negix (make-interval -7 -3))
(define mixix (make-interval -7 3))

(define posiy (make-interval 1 5))
(define negiy (make-interval -5 -1))
(define mixiy (make-interval -1 5))

(mul-interval posix posiy)
(mul-interval posix negiy)
(mul-interval posix mixiy)
(newline)
(mul-interval negix posiy)
(mul-interval negix negiy)
(mul-interval negix mixiy)
(newline)
(mul-interval mixix posiy)
(mul-interval mixix negiy)
(mul-interval mixix mixiy)