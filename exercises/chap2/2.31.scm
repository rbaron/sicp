; In this exercise we should abstract the 2.30 `square-tree` procedure
; in a `tree-map` procedure, so we can map arbitrary procedures on trees.

(define (tree-map proc tree)
  (cond ((null? tree) ())
        ((not (pair? tree)) (proc tree))
        (else (cons (tree-map proc (car tree))
                    (tree-map proc (cdr tree))))))

; Let's define `suare-tree` in terms of `tree-map`:
(define (square-tree tree) (tree-map square tree))

; Testing:
(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
; => (1 (4 (9 16) 25) (36 49))
