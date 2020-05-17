#lang sicp
(#%require sicp-pict)

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (frame-origin f) (car f))
(define (frame-edge1 f) (cadr f))
(define (frame-edge2 f) (caddr f))


(define (make-frame-alt origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (frame-origin-alt f) (car f))
(define (frame-edge1-alt f) (cadr f))
(define (frame-edge2-alt f) (cddr f))

(define exframe (make-frame-alt 1 2 3))
(frame-origin-alt exframe)
(frame-edge1-alt exframe)
(frame-edge2-alt exframe)