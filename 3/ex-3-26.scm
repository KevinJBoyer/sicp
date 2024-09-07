#lang sicp

; pg 367


; each node is null or ((key value) (left-branch right-branch))
; where left-branch and right-branch are themselves nodes
(define (new-node key value) (cons (cons key value) (cons '() '())))

(define entry car)
(define set-entry! set-car!)
(define get-key caar)
(define (get-value node) (cdr (car node)))
(define (set-value! node value) (set-entry! node (cons (get-key node) value)))

(define get-branches cdr)
(define (left-branch node) (car (get-branches node)))
(define (right-branch node) (cdr (get-branches node)))
(define (set-left-branch! node new-left-branch) (set-car! (cdr node) new-left-branch))
(define (set-right-branch! node new-right-branch) (set-cdr! (cdr node) new-right-branch))

(define make-tree (cons '() '()))

(define (insert! node key value)
 (cond
   ; Empty tree?
   ((null? (entry node))
    (set-entry! node (cons key value))
    (set-cdr! node (cons '() '())) ; init branches to empty
    )

   ; Or, this node is the one we're looking for
   ((eq? key (get-key node)) (set-value! node value))
   
   ; if key is less than, insert as left branch
   ; or recurse into left branch
   ((< key (get-key node))
    (if (null? (left-branch node))
        (set-left-branch! node (new-node key value))
        (insert! (left-branch node) key value)))
 
   ; same for right branch
   ((> key (get-key node))
    (if (null? (right-branch node))
        (set-right-branch! node (new-node key value))
        (insert! (right-branch node) key value))))
     
 'ok)

(define (lookup node key)
 (cond
   ((null? (entry node)) false) ; empty tree
   ((eq? key (get-key node)) (get-value node))
   ((< key (get-key node)) (lookup (left-branch node) key))
   ((> key (get-key node)) (lookup (right-branch node) key))
   ))

; testing
(let
    ((some-tree make-tree))
  
   (insert! some-tree 2 'x-value)
   (insert! some-tree 2 'b-value) ; ((2 . b-value) ())
   (insert! some-tree 3 'c-value) ; ((2 . b-value) () (3 . c-value) ())
   (insert! some-tree 1 'a-value) ; ((2 . b-value) ((1 . a-value) ()) (3 . c-value) ())
   (insert! some-tree 3 'x-value) ; ((2 . b-value) ((1 . a-value) ()) (3 . x-value) ())
   (lookup some-tree 1)
  )