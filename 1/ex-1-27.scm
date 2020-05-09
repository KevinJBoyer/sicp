#lang scheme

(define (square-test n p)
  (cond ((and
          (not (= n 1))
          (not (= n (- p 1)))
          (= (remainder (* n n) p) 1))
          0)
        (else (* n n))))

(define (prime? n) (miller-rabin? n 100))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square-test (expmod base (/ exp 2) m) m)
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (mr-test n)
  (define (try-it a)
    (> (expmod a (- n 1) n) 0))
  (try-it (+ 1 (random (- n 1)))))

(define (miller-rabin? n times)
  (cond ((= times 0) true)
        ((mr-test n) (miller-rabin? n (- times 1)))
        (else false)))

(prime? 1105) ;carmichael
(prime? 16) ;not prime
(prime? 2821) ;charmichael
(prime? 27644437) ;prime

(define (report-prime n exp)
  (cond ((or
          (and exp (prime? n))
          (and (not exp) (not (prime? n))))
         (newline))
        (else (display "error"))))

(report-prime 2 true)  
 (report-prime 7 true)  
 (report-prime 13 true)  
 (report-prime 15 false) 
 (report-prime 37 true)

 (report-prime 2 true)  
 (report-prime 7 true)  
 (report-prime 13 true)  
 (report-prime 15 false) 
 (report-prime 37 true)  
 (report-prime 39 false) 
   
 (report-prime 561 false)  ; Carmichael number  
 (report-prime 1105 false) ; Carmichael number  
 (report-prime 1729 false) ; Carmichael number  
 (report-prime 2465 false) ; Carmichael number  
 (report-prime 2821 false) ; Carmichael number  
 (report-prime 6601 false) ; Carmichael number 