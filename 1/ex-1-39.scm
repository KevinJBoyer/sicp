#lang scheme
(define (cont-frac-r n d k)
  (define (cont-frac-iter i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i)
                    (cont-frac-iter (+ 1 i))))))
  (cont-frac-iter 1))

(define (cont-frac n d k)
  (define (cont-frac-iter i result)
    (if (= i 0)
        result
        (cont-frac-iter (- i 1) (/ (n i) (+ (d i) result)))))
  (cont-frac-iter k 0))

; why are these different?
(cont-frac-r
           (lambda (i) 1.0)
           (lambda (i) (if (= 2 (remainder i 3)) (* (/ 2 3) (+ i 1)) 1))
           5000)

(cont-frac
           (lambda (i) 1.0)
           (lambda (i) (if (= 2 (remainder i 3)) (* (/ 2 3) (+ i 1)) 1))
           5000)

(define (tan-cf x k)
  (cont-frac
        (lambda (n) (if (= n 1) x (- 0 (* x x))))
        (lambda (d) (- (* 2 d) 1.0))
        k))

(tan-cf 90 100)