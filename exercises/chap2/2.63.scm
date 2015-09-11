; Let's analyze two versions of tree-to-list procedures.

; Version 1.
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

; Version 2.
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

; In this version, we can see how the recursion first visit
; the left branches of the treee, and append that result
; to the list consisting of the current entry and the recursive
; call on the right list.

; Let's define the trees in image 2.16:

; Given procedures:
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

; First tree from Figure 2.16
(define t1
  (make-tree 7
    (make-tree 3
      (make-tree 1 '() '())
      (make-tree 5 '() '()))
    (make-tree 9
      '()
      (make-tree 11 '() '()))))

; Second tree from Figure 2.16
(define t2
  (make-tree 3
    (make-tree 1 '() '())
    (make-tree 7
      (make-tree 5 '() '())
      (make-tree 9
        '()
        (make-tree 11 '() '())))))

; Third tree from Figure 2.16
(define t3
  (make-tree 5
    (make-tree 3
      (make-tree 1 '() '())
      '())
    (make-tree 9
      (make-tree 7 '() '())
      (make-tree 11 '() '()))))

; a. Do the two procecures produce the same result for every tree?

; Tree 1:
(tree->list-1 t1)
(tree->list-2 t1)
; => Both yield the same result: (1 3 5 7 9 11)

; Tree 2:
(tree->list-1 t2)
(tree->list-2 t2)
; => Both yield the same result: (1 3 5 7 9 11)

; Tree 3:
(tree->list-1 t3)
(tree->list-2 t3)
; => Both yield the same result: (1 3 5 7 9 11)

; Answer: Yes, since both traverse the tree "in-order".

; b. Do the two procedures have the same order of growth?

; `tree->list-1` calls `append` on the result of the recursive call. `append`
; has to traverse its first argument to find the end of it, so it can point it
; to the beginning of the second argument.

; Writing the time complexity equation, we have:

; T(n) = 2*T(n/2) + n/2

; Which is the same recursion as famous sorting algorithms.
; Solving it yeilds a time complexity O(nlogn).

; `tree->list-2` avoids the `n` factor by avoiding calls to `append`. It equation
; lacks the `n` term:

; T(n) = 2*T(n/2) + constant

; Which is the characteristic time complexity of a simple tree traversal, since
; the time complexity of each visited state is constant. A solution for it is
; O(logn)
