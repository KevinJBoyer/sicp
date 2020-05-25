#lang sicp

(#%require "type-table.scm")

(define (raise x) (apply-generic 'raise x))
(define (project x) (apply-generic 'project x))

(define (level x)
  (define (iter next-x n)
    (if (not (get 'raise (list (type-tag next-x))))
        n
        (iter (raise next-x) (+ n 1))))
  (iter x 0))

(define (drop x)
  (if (and (get 'project (list (type-tag x)))
           (equal? x (raise (project x))))
      (drop (project x))
      x))

(define (apply-generic op . args)

  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (and (= 2 (length args))
                   (not (and (= 0 (level (car args)))
                             (= 0 (level (cadr args))))))
              (cond ((> (level (car args)) (level (cadr args)))
                     (apply-generic op (raise (car args)) (cadr args)))
                    (else
                     (apply-generic op (car args) (raise (cadr args)))))
              (error "No method for these types" (list op type-tags)))))))

(#%provide level)
(#%provide drop)
(#%provide apply-generic)