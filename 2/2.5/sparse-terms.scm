#lang sicp

(#%require "type-table.scm")
(#%require "arithmetic-system.scm")

(define (install-sparse-terms)

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  
  (define (neg-termlist term-list)
    (if (null? term-list)
        (the-empty-termlist)
        (adjoin-term (make-term (order (first-term term-list))
                                (mul -1 (coeff (first-term term-list))))
                     (neg-termlist (rest-terms term-list)))))

  (define (sub-terms L1 L2) (add-terms L1 (neg-termlist L2)))

  (define (remainder-terms L1 L2) (cadr (div-terms L1 L2)))
  (define (psuedoremainder-terms L1 L2)
    (define (integerizing-factor p q)
      (expt (coeff (first-term q))
            (inc (sub (order (first-term p)) (order (first-term q))))))
    (remainder-terms
     (mul-term-by-all-terms (make-term 0 (integerizing-factor L1 L2)) L1)
     L2))

  
  (define (gcd-terms L1 L2)
    ;(display "gcd terms: ")(display (list L1 L2))(newline)
    (if (empty-termlist? L2)
        (simplify-coeffs L1)
        (gcd-terms L2 (psuedoremainder-terms L1 L2))))

  (define (simplify-coeffs L)
    (let ((gcd-coeffs (gcd-list (map coeff L))))
      (map (lambda (term)
             (make-term (order term) (div (coeff term) gcd-coeffs)))
           L)))
  
  (define (gcd-list L)
    (define (gcd-list remaining-items curr-gcd)
      (if (null? remaining-items)
          curr-gcd
          (gcd-list (cdr remaining-items) (gcd (car remaining-items) curr-gcd))))
     (gcd-list L (car L)))


  (define (reduce-terms L1 L2)
    (let ((gcd-of-terms (gcd-terms L1 L2)))
      (list (car (div-terms L1 gcd-of-terms))
            (car (div-terms L2 gcd-of-terms)))))
  
    (define (add-terms L1 L2)
      (cond ((empty-termlist? L1) L2)
            ((empty-termlist? L2) L1)
            (else
             (let ((t1 (first-term L1))
                   (t2 (first-term L2)))
               (cond ((> (order t1) (order t2))
                      (adjoin-term
                       t1 (add-terms (rest-terms L1) L2)))
                     ((< (order t1) (order t2))
                      (adjoin-term
                       t2 (add-terms L1 (rest-terms L2))))
                     (else
                      (adjoin-term
                       (make-term (order t1)
                                  (add (coeff t1) (coeff t2)))
                       (add-terms (rest-terms L1)
                                  (rest-terms L2)))))))))

    (define (mul-terms L1 L2)
      (if (empty-termlist? L1)
          (the-empty-termlist)
          (add-terms (mul-term-by-all-terms (first-term L1) L2)
                     (mul-terms (rest-terms L1) L2))))
    (define (mul-term-by-all-terms t1 L)
      (if (empty-termlist? L)
          (the-empty-termlist)
          (let ((t2 (first-term L)))
            (adjoin-term
             (make-term (+ (order t1) (order t2))
                        (mul (coeff t1) (coeff t2)))
             (mul-term-by-all-terms t1 (rest-terms L))))))

    (define (div-terms L1 L2)
      (if (empty-termlist? L1)
          (list (the-empty-termlist) (the-empty-termlist))
          (let ((t1 (first-term L1))
                (t2 (first-term L2)))
            (if (> (order t2) (order t1))
                (list (the-empty-termlist) L1)
                (let ((new-c (div (coeff t1) (coeff t2)))
                      (new-o (- (order t1) (order t2))))
                  (let ((rest-of-result
                         (div-terms
                          (sub-terms L1
                                     (mul-term-by-all-terms (make-term new-o new-c) L2))
                          L2)
                         ))
                    (list
                     (adjoin-term (make-term new-o new-c) (car rest-of-result))
                     (cadr rest-of-result)
                     )))))))
  
    (define (tag x) (attach-tag 'sparse-terms x))

    (put 'reduce '(sparse-terms sparse-terms) (lambda (L1 L2) (map tag (reduce-terms L1 L2))))
    (put 'add-terms '(sparse-terms sparse-terms) (lambda (L1 L2) (tag (add-terms L1 L2))))
    (put 'mul-terms '(sparse-terms sparse-terms) (lambda (L1 L2) (tag (mul-terms L1 L2))))
    (put 'gcd-terms '(sparse-terms sparse-terms) (lambda (L1 L2) (tag (gcd-terms L1 L2))))
    (put 'div-terms '(sparse-terms sparse-terms)
         (lambda (L1 L2)
           (list
            (tag (car (div-terms L1 L2)))
            (tag (cadr (div-terms L1 L2)))

            )))
    (put 'neg-termlist '(sparse-terms) (lambda (L) (tag (neg-termlist L))))
    (put 'empty-termlist? '(sparse-terms) (lambda (L) (empty-termlist? L)))
    (put 'make 'sparse-terms (lambda (L) (tag L))))

  (install-sparse-terms)