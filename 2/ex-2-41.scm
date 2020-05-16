#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval start n)
  (define (iter i)
    (if (> i n)
        nil
        (cons i (iter (+ i 1)))))
  (iter start))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
         ((predicate (car sequence))
          (cons (car sequence) (filter predicate (cdr sequence))))
         (else (filter predicate (cdr sequence)))))

(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map
      (lambda (j) (list i j))
      (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (ordered-triples n)
  (if (< n 3)
      nil
      (append (map
               (lambda (k) (cons n k))
               (unique-pairs (- n 1)))
              (ordered-triples (- n 1)))))

(define (sum-triple t) (+ (car t) (cadr t) (caddr t)))

(define (triple-sums-to s n)
  (filter
   (lambda (t) (= (sum-triple t) s))
   (ordered-triples n)))

(triple-sums-to 10 15)