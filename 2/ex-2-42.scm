#lang sicp
   
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
         ((predicate (car sequence))
          (cons (car sequence) (filter predicate (cdr sequence))))
         (else (filter predicate (cdr sequence)))))

(define (enumerate-interval start n)
  (define (iter i)
    (if (> i n)
        nil
        (cons i (iter (+ i 1)))))
  (iter start))

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

;;;


(define (queens board-size)
  (define empty-board nil)
  
  (define (adjoin-position new-row new-column rest-of-queens)
    (append rest-of-queens (list (list new-row new-column))))



  
  (define (safe? new-queen-column positions)
    ; positions are in ((row, column), ...)
    (define (checked-positions init-row init-col)
      (define (iter offset)
        (if (= offset 0)
            nil
            (cons   (list init-row (+ init-col offset))
                    (cons  (list (+ init-row offset) (+ init-col offset))
                           (cons (list (- init-row offset) (+ init-col offset))
                                 (iter (+ offset 1)))))))
      (iter (- 1 init-col)))

    (define (intersection list1 list2)
      (define (are-equal? x y)
        (and (= (car x) (car y))
             (= (cadr x) (cadr y))))
  
      (define (in-list? list item)
        (cond ((null? list) false)
              ((are-equal? item (car list)) true)
              (else (in-list? (cdr list) item))))
      (filter
       (lambda (list-item) (in-list? list2 list-item))
       list1))

    (define (last l) (if (null? (cdr l)) (car l) (last (cdr l))))
    
    (null? (intersection positions (checked-positions
                                    (car (last positions)) ; new-queen-row
                                    new-queen-column))))
  
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))

  (queen-cols board-size))

(queens 5)