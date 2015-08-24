; In this exercise we are supposed to write two versions of the `square-tree`
; procedure. One should be implemented without any high-order procedure (such
; as `map`) and the other should use it.

; Implementation #1
; Reasoning: the square of a tree is the tree formed by the square
; of its subtrees.

(define (square-tree tree)
  (cond ((null? tree) ())
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

; Trying this out:
(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
; => (1 (4 (9 16) 25) (36 49))


; Implementation #2
; Reasoning: we now can `map` the `square-tree` procedure
; on subtrees:

(define (square-tree tree)
  (cond ((null? tree) ())
        ((not (pair? tree)) (* tree tree))
        (else (map (lambda (x) (square-tree x)) tree))))

; Trying this out:
(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
; => (1 (4 (9 16) 25) (36 49))
