#lang sicp

(define (test name got expected)
  (cond ((not (equal? got expected))
         (display "test failed: ")
         (display name)
         (newline)
         (display "expected: ")
         (display expected)
         (newline)
         (display "     got: ")
         (display got)
         (newline)
         (newline))))

(#%provide test)