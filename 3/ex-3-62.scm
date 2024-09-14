#lang sicp

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x) (newline) (display x))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (square n) (* n n))

(define (prime? n)
  (define (smallest-divisor n)
    (find-divisor n 2))
  
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  
  (define (divides? a b)
    (= (remainder b a) 0))

  (= n (smallest-divisor n)))

; testing by looking for the second prime between 10,000 and 1,000,000
;(stream-car
; (stream-cdr
;  (stream-filter prime?
;                 (stream-enumerate-interval
;                  10000 1000000))))

(define (stream-map-old proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map-old proc (stream-cdr s)))))

; pg 440
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

; testing
;(display-stream (stream-map +
;                            (cons-stream 5 (cons-stream 1 the-empty-stream))
;                            (cons-stream 12 (cons-stream 5 the-empty-stream))
;                            (cons-stream 3 (cons-stream 7 the-empty-stream))))


(define (add-streams s1 s2) (stream-map + s1 s2))
(define (mul-streams s1 s2) (stream-map * s1 s2))

(define ones (cons-stream 1 ones))
(define integers
  (cons-stream 1 (add-streams ones integers)))


(define factorials
  (cons-stream 1 (mul-streams
                  (stream-cdr integers)
                  factorials
                  )))

(define (stream-print-first s n)
  (if (> n 0)
      (begin
        (display (stream-car s))
        (display " ")
        (stream-print-first (stream-cdr s) (- n 1)))))

;(stream-print-first factorials 5)


(define (partial-sums s)
  (cons-stream
   (stream-car s)
   (add-streams (partial-sums s) (stream-cdr s))))


(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream
                   s2car
                   (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1)
                          (stream-cdr s2)))))))))


(define (scale-stream s n)
(cons-stream
  (* (stream-car s) n)
  (scale-stream (stream-cdr s) n)))

;(define S (cons-stream 1 (merge (merge (scale-stream S 2) (scale-stream S 3)) (scale-stream S 5))))
;(stream-print-first S 25)

(define (stream-from-list list)
  (if (null? list)
      the-empty-stream
      (cons-stream (car list) (stream-from-list (cdr list)))))

(define (integrate-series s)
  (define (integrate-series-from-n s n)
    (cons-stream (/ (stream-car s) n)
                 (integrate-series-from-n (stream-cdr s) (+ n 1))))
  (integrate-series-from-n s 1))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))


(define negative-ones (cons-stream -1 negative-ones))

(define cosine-series
  (cons-stream
   1
   (mul-streams negative-ones (integrate-series sine-series))
   ))

(define sine-series
  (cons-stream
   0
   (integrate-series cosine-series)
   ))
  
;(stream-print-first cosine-series 7)
;(display-line " ")
;(stream-print-first sine-series 7)

(define add-series add-streams)

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams
                (scale-stream (stream-cdr s1) (stream-car s2))
                (mul-series s1 (stream-cdr s2))

                )))

(define (invert-unit-series s)
  (cons-stream
   1
   (scale-stream
    (mul-series (stream-cdr s) (invert-unit-series s))
    -1)))


(define (div-series s1 s2)
  (mul-series
   (scale-stream s1 (/ 1 (stream-car s2)))

   (invert-unit-series (to-unit-series s2))))
   

(define (to-unit-series s)
  (scale-stream s (/ 1 (stream-car s))))

;testing
(define some-series (stream-from-list (list 2 4 16 32 64 128 256)))
(define some-other-series (stream-from-list (list 3 5 7 11 13 17)))

(define divided (div-series some-series some-other-series))
(stream-print-first divided 5)
(display-line "Remultiplied: ")
(stream-print-first (mul-series divided some-other-series) 5)

(define tan-series (div-series sine-series cosine-series))
(display-line "Tangent series: ")
(stream-print-first tan-series 10)