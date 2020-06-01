#lang sicp

(define (make-account balance secret)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  
  (define (wrong-password x) "Incorrect password.")
  
  (define (dispatch pw m)
    (cond ((not (eq? pw secret)) wrong-password)
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT" m))))
  dispatch)

(define (make-joint acc old-pw new-pw)
  (lambda (try-pw msg)
    (if (eq? try-pw new-pw)
        (acc old-pw msg)
        "Incorrect password for joint account.")))

(define alison-acc (make-account 50 'apple))
(define kevin-fail (make-joint alison-acc 'bad-apple 'banana))
(define kevin-jacc (make-joint alison-acc 'apple 'banana))
((alison-acc 'apple 'deposit) 40)
((kevin-jacc 'banana 'withdraw) 10)
;((kevin-fail 'orange 'withdraw) 10)
;((kevin-jacc 'orange 'withdraw) 10)