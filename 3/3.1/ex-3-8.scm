#lang sicp

(define f
  (let ((prev 0))
    (lambda (n)
      (cond ((and (= n 0) (= prev 0)) (set! prev 1) -1)
            ((and (= n 1) (= prev 1)) 1)
            
            ((and (= n 1) (= prev 0)) (set! prev 1) 0)
            ((and (= n 0) (= prev 1)) 1)))))

;(+ (f 0) (f 1))
(+ (f 1) (f 0))