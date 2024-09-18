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

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0
     (stream-map (lambda (guess) (sqrt-improve guess x))
                 guesses)))
  guesses)

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(define (stream-limit s tolerance)
  (if (< (abs (- (stream-car s) (stream-car (stream-cdr s)))) tolerance)
      (stream-car (stream-cdr s))
      (stream-limit (stream-cdr s) tolerance)))

;(stream-print-first (sqrt-stream 2) 5)
;(sqrt 2 .01)

(define (euler-transform s)
  (let ((s0 (stream-ref s 0)) ; Sn-1
        (s1 (stream-ref s 1)) ; Sn
        (s2 (stream-ref s 2))) ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(define (ln-2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln-2-summands ( + n 1)))))

(define ln-2-stream
  (partial-sums (ln-2-summands 1)))

(define (make-tableau transform s)
(cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

;;;
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs-i-less-j s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs-i-less-j (stream-cdr s) (stream-cdr t)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (interleave
     (stream-map (lambda (x) (list x (stream-car t) ))
                 (stream-cdr s))
     (pairs (stream-cdr s) (stream-cdr t))))))

;(stream-print-first (pairs-i-less-j integers integers) 10)
;(display-line "")
;(stream-print-first (pairs integers integers) 10)


(define (pairs-bad s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
               t)
   (pairs-bad (stream-cdr s) (stream-cdr t))))

;(stream-print-first (pairs-bad integers integers) 10)


;;;

(define (triples s t u)
  (cons-stream
   (cons (stream-car s) (stream-car (pairs t u)))
   (interleave
    (stream-map (lambda (x) (cons (stream-car s) x)) (stream-cdr (pairs t u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define (is-pythagorean-triple? triple)
  (let ((a (car triple)) (b (cadr triple)) (c (caddr triple)))
    (= (* c c) (+ (* a a) (* b b))))) 

(define pythagorean-triples (stream-filter is-pythagorean-triple? (triples integers integers integers)))
;(stream-print-first pythagorean-triples 5)


;;;

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< (weight s1car s2car) 0)
                  (cons-stream
                   s1car
                   (merge-weighted (stream-cdr s1) s2 weight)))
                 ((> (weight s1car s2car) 0)
                  (cons-stream
                   s2car
                   (merge-weighted s1 (stream-cdr s2) weight)))
                 (else
                  (cons-stream
                   s1car
(cons-stream s2car
                   (merge-weighted
                    (stream-cdr s1)
                    (stream-cdr s2)
                    weight)))))))))

(define (weight-by-sum s1-pair s2-pair)
  (let ((s1-pair-sum (+ (car s1-pair) (cadr s1-pair)))
        (s2-pair-sum (+ (car s2-pair) (cadr s2-pair))))
    (cond ((< s1-pair-sum s2-pair-sum) -1)
          ((> s1-pair-sum s2-pair-sum) 1)
          (else 0))))

(define (weight-by-weird-formula s1-pair s2-pair)
  (define (pair-weight pair)
    (+ (* 2 (car pair)) (* 3 (cadr pair)) (* 5 (car pair) (cadr pair))))
  (let ((s1-pair-weight (pair-weight s1-pair))
        (s2-pair-weight (pair-weight s2-pair)))
    (cond ((< s1-pair-weight s2-pair-weight) -1)
          ((> s1-pair-weight s2-pair-weight) 1)
          (else 0))))


; ex 3.70

(define (weighted-pairs-i-less-j s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs-i-less-j (stream-cdr s) (stream-cdr t))
    weight
    )))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t)) ; (1 1)
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t)) ; (1 2,3,4,5...)
    (merge-weighted
     (stream-map (lambda (x) (list x (stream-car t)))
                 (stream-cdr s)) ; (2,3,4,5... 1)
     (weighted-pairs (stream-cdr s) (stream-cdr t) weight) ; (2 2)...
     weight)
    weight)))

;(stream-print-first (weighted-pairs-i-less-j integers integers weight-by-sum) 10)

(define (not-divisible-by-2-3-5-num? num)
  (not (or
   (= 0 (remainder num 2))
   (= 0 (remainder num 3))
   (= 0 (remainder num 5)))))

(define (not-divisible-by-2-3-5? pair)
  (and
   (not-divisible-by-2-3-5-num? (car pair))
   (not-divisible-by-2-3-5-num? (cadr pair))))


(stream-print-first
 (stream-filter
  not-divisible-by-2-3-5?
  (weighted-pairs-i-less-j integers integers weight-by-weird-formula)
  )
 10)