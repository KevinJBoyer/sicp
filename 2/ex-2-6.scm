#lang scheme

(define zero
  (lambda (f)
    (lambda (x) x)
    )
  )

(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

(define one
  (lambda (f)
    (lambda (x)
      (f x)))
  )

(define two
    (lambda (f)
      (lambda (x)
        (f (f x))))
  )

; this is incorrect
; skipping for now or else I'll lose my mind :'-)
(define (add a b)
  (a b))

(add one two)