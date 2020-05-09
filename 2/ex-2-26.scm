#lang sicp

(define x (list 1 2 3))
(define y (list 4 5 6))

; (1 2 3 4 5 6)
(append x y)

; I thought:
; ((1 2 3) . (4 5 6))
; ...but no! I didn't think through that (4 5 6) is really (4 (5 (6 nil)))
; so in fact it is ((1 2 3) 4 5 6)
(cons x y)

; ((1 2 3) (4 5 6))
(list x y)