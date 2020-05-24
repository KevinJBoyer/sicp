#lang sicp

(#%require "type-table.scm")


(define (apply-generic op . args)
  (define (raise x) (apply-generic 'raise x))

  (define (raise-level x)
    (define (iter y n)
      (let ((next (get 'raise (list (type-tag y)))))
        (if next
            (iter (next (contents y)) (- n 1))
            n)))
    (iter x 0))
  
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((level1 (raise-level (car args)))
                    (level2 (raise-level (cadr args))))
                (cond ((and (= level1 0) (= level2 0))
                       (error "No method for these types" (list op type-tags)))
                      ((< level1 level2)
                       (apply-generic op (raise (car args)) (cadr args)))
                      ((> level1 level2)
                       (apply-generic op (car args) (raise (cadr args))))
                      (else
                       (apply-generic op (raise (car args)) (raise (cadr args))))))
              (error "No method for these types" (list op type-tags)))))))

(#%provide apply-generic)
