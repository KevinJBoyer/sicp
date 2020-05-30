#lang sicp

(#%require "type-table.scm")
(#%require "arithmetic-system.scm")

(define (install-dense-terms)
  
  (define (neg-termlist term-list)
    (if (null? term-list)
        '()
        (cons (mul -1 (car term-list)) (neg-termlist (cdr term-list)))))

  (define (empty-termlist? L) (null? L))
  
  (define (add-terms L1 L2)
    (cond ((> (length L1) (length L2))
           (cons (car L1) (add-terms (cdr L1) L2)))
          ((< (length L1) (length L2))
           (cons (car L2) (add-terms L1 (cdr L2))))
          ((< 0 (length L1))
           (cons (add (car L1) (car L2)) (add-terms (cdr L1) (cdr L2))))
          (else '())))

  (define (mul-terms L1 L2)
    (define (iter multiplicand multiplier-full order)
      (define (append-zeroes num-zeros L)
        (if (= 0 num-zeros) L (append-zeroes (dec num-zeros) (append L '(0)))))

      (define (mul-terms-by-term multiplicand-terms multiplier-term)
        (if (null? multiplicand-terms)
            '()
            (cons (mul (car multiplicand-terms) multiplier-term)
                  (mul-terms-by-term (cdr multiplicand-terms) multiplier-term))))
      
      (if (> 0 order)
          '()
          (add-terms
           (append-zeroes order (mul-terms-by-term multiplicand (car multiplier-full)))
           (iter multiplicand (cdr multiplier-full) (dec order)))))
    
    (if (or (null? L1) (null? L2))
        '()
        (iter L1 L2 (dec (length L2)))))
 
  (define (tag x) (attach-tag 'dense-terms x))

  (put 'add-terms '(dense-terms dense-terms) (lambda (L1 L2) (tag (add-terms L1 L2))))
  (put 'mul-terms '(dense-terms dense-terms) (lambda (L1 L2) (tag (mul-terms L1 L2))))
  (put 'neg-termlist '(dense-terms) (lambda (L) (tag (neg-termlist L))))
  (put 'empty-termlist? '(dense-terms) (lambda (L) (empty-termlist? L)))
  (put 'make 'dense-terms (lambda (L) (tag L))))

(install-dense-terms)