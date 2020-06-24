#lang sicp

; nodes with contents, next, prev (for deque)
(define (node-make prev next contents) (cons prev (cons next contents)))
(define (node-prev n) (car n))
(define (set-node-prev! n x) (set-car! n x))
(define (node-next n) (cadr n))
(define (set-node-next! n x) (set-car! (cdr n) x))
(define (node-contents n) (cddr n))
(define (set-node-contents! n x) (set-cdr! (cdr n) x))

(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))
(define (empty-deque? deque) (null? (front-ptr deque)))
(define (make-deque) (cons '() '()))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (node-contents (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (node-contents (rear-ptr deque))))

;done...?
(define (front-insert-deque! deque item)
  (let ((new-node (node-make '() (front-ptr deque) item)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-node)
           (set-rear-ptr! deque new-node)
           new-node)
          (else
           (set-node-prev! (front-ptr deque) new-node)
           (set-front-ptr! deque new-node)
           new-node))))

(define (rear-insert-deque! deque item)
  (let ((new-node (node-make (rear-ptr deque) '() item)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-node)
           (set-rear-ptr! deque new-node)
           new-node)
          (else
           (set-node-next! (rear-ptr deque) new-node)
           (set-rear-ptr! deque new-node)
           new-node))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "FRONT-DELETE! called with an empty deque" deque))

        ; only one item before delete?
        ((null? (node-next (front-ptr deque)))
         (set-front-ptr! deque '())
         (set-rear-ptr! deque '())
         deque)

        ; only one item after delete?
        ((null? (node-next (node-next (front-ptr deque))))
         (set-front-ptr! deque (node-next (front-ptr deque)))
         (set-rear-ptr! deque (node-next (front-ptr deque)))
         (set-node-prev! (front-ptr deque) '())
         deque)
        
        (else
         (set-front-ptr! deque (node-next (front-ptr deque)))
         (set-node-prev! (front-ptr deque) '())
         deque)))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "REAR-DELETE! called with an empty deque" deque))

        ; only one item before delete?
        ((null? (node-prev (rear-ptr deque)))
         (set-front-ptr! deque '())
         (set-rear-ptr! deque '())
         deque)

        ; only one item after delete?
        ((null? (node-prev (node-prev (rear-ptr deque))))
         (set-front-ptr! deque (node-prev (rear-ptr deque)))
         (set-rear-ptr! deque (node-prev (rear-ptr deque)))
         (set-node-next! (front-ptr deque) '())
         deque)
        
        (else
         (set-rear-ptr! deque (node-prev (rear-ptr deque)))
         (set-node-next! (rear-ptr deque) '())
         deque)))

;testing
(define z (make-deque))
(front-insert-deque! z 5)
(front-insert-deque! z 3)
(front-insert-deque! z 1)
(rear-insert-deque! z 7)
(front-deque z)
(rear-deque z)
(front-delete-deque! z) ; after: 3 5 7
(front-deque z)
(front-delete-deque! z) ; after: 5 7
(front-deque z)
(front-delete-deque! z) ; after: 7
(front-deque z)
(front-delete-deque! z) ; after: '() '()
;(front-deque z)

(newline)
(front-insert-deque! z 5)
(front-insert-deque! z 3)
(front-insert-deque! z 1)
(rear-insert-deque! z 7)
(rear-deque z)
(rear-delete-deque! z) ; after: 1 3 5
(rear-deque z)
(rear-delete-deque! z) ; after: 1 3
(rear-deque z)
(rear-delete-deque! z) ; after: 1
(rear-deque z)
(rear-delete-deque! z) ; after: '() '()
;(rear-deque z)
