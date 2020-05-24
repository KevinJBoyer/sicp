#lang sicp

(#%require "type-table.scm")

(define (apply-generic op . args)
  
  (define (apply-iter remaining-args)
    (define (available-coercion from-type)
      (let ((to-type (type-tag (car remaining-args))))
        (if (get-coercion from-type to-type) to-type from-type)))

    (define (coerce-if-available arg)
      (if (eq? (type-tag arg) (available-coercion (type-tag arg)))
          (contents arg)
          (contents ((get-coercion (type-tag arg) (type-tag (car remaining-args))) arg))))
    
    (if (null? remaining-args)
        (error "No method for these types" (list op (map type-tag args)))
        (let ((type-tags-from (map type-tag args)))
          (let ((type-tags-to (map available-coercion type-tags-from)))
            (let ((proc (get op type-tags-to)))
              (if proc
                  (apply proc (map coerce-if-available args))
                  (apply-iter (cdr remaining-args))))))))
  
  (let ((proc (get op (map type-tag args))))
    (if proc
        (apply proc (map contents args))
        (apply-iter args))))

(#%provide apply-generic)
