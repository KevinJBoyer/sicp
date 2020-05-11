#lang sicp

(define (make-mobile left right)
  (list left right))

(define (left-branch m) (car m))
(define (right-branch m) (car (cdr m)))


(define (make-branch length structure)
  (list length structure))

(define (branch-length b) (car b))
(define (branch-structure b) (car (cdr b)))

(define (total-weight m)
  (let ((l (branch-structure (left-branch m)))
        (r (branch-structure (right-branch m))))
    (+
     (if (pair? l) (total-weight l) l)
     (if (pair? r) (total-weight r) r))))

(define (balanced? m)
  (define (torque b)
    (* (branch-length b)
       (if (pair? (branch-structure b))
           (total-weight (branch-structure b))
           (branch-structure b))))
  
  (if (pair? m)
      (and (= (torque (left-branch m))
              (torque (right-branch m)))
           (balanced? (branch-structure (left-branch m)))
           (balanced? (branch-structure (right-branch m))))
      true))
       

; some tests
(define mobile-a (make-mobile
                  (make-branch 2 3)
                  (make-branch 1 6)))
(define mobile-b (make-mobile
                  (make-branch 2 30)
                  (make-branch 1 60)))
(define mobile-c (make-mobile
                  (make-branch 10 mobile-a)
                  (make-branch 1 mobile-b)))
(define mobile-d (make-mobile
                  (make-branch 1 9)
                  (make-branch 2 mobile-a)))

;(total-weight mobile-a)
;(total-weight mobile-b)
;(total-weight mobile-c)
;(total-weight mobile-d)

(balanced? mobile-a)
(balanced? mobile-b)
(balanced? mobile-c)
(balanced? mobile-d)