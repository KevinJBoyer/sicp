#lang sicp

(define (call-the-cops) "cops called")

(define (make-account balance secret)
  (let ((consec-wrong 0))
    
    (define (withdraw amount)
      (set! consec-wrong 0)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    
    (define (deposit amount)
      (set! consec-wrong 0)
      (set! balance (+ balance amount))
      balance)

    (define (wrong-password x)
      (if (= consec-wrong 7)
          (call-the-cops)
          (begin (set! consec-wrong (inc consec-wrong)) "Incorrect password.")))
  
    (define (dispatch pw m)
      (cond ((not (eq? pw secret)) wrong-password)
            ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch))

(define a (make-account 5 'banana))
((a 'apple 'withdraw) 7)
((a 'apple 'withdraw) 7)
((a 'apple 'withdraw) 7)
((a 'apple 'withdraw) 7)
((a 'apple 'withdraw) 7)
((a 'apple 'withdraw) 7)
((a 'apple 'withdraw) 7)
((a 'banana 'withdraw) 7)
((a 'apple 'withdraw) 7)
((a 'apple 'withdraw) 7)
((a 'apple 'withdraw) 7)
((a 'apple 'withdraw) 7)
((a 'apple 'withdraw) 7)
((a 'apple 'withdraw) 7)
((a 'apple 'withdraw) 7)
((a 'apple 'withdraw) 7)