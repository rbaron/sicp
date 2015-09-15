; The order of growth of `encode-symbol` from 2.68.

; The `encode-symbol` procedure was developed as follows:
(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((element-of-set? symbol (symbols (right-branch tree)))
          (cons 1 (encode-symbol symbol (right-branch tree))))
        ((element-of-set? symbol (symbols (left-branch tree)))
          (cons 0 (encode-symbol symbol (left-branch tree))))
        (else
          (error "Symbol not found in tree -- " symbol))))

; Given a tree, the procedure tries to find the given symbol on
; it's right and left children trees. As we saw on 2.71, the whole
; tree has a depth of n-1. To check whether a symbol is contained
; on a set, even with the representation using ordered lists, we
; need a number of steps that is linear n n.

; So, in the worst case scenario, we'd visit all the n-1 nodes
; from the tree. For the first node, we'd waste at most n steps
; to verify that the symbol is in the set. At the next node, we'd
; need n-1 steps and so on. That gives us a factor that is proportinal
; to n^2. The complexity is, then, proportional to n^2, or O(n^2).

; In the best case scenario, we'd be trying to encode the most frequent
; symbol. If we choose to look for the symbol in the left branch first,
; we will "waste" n-1 steps, since the symbol is in the right branch. This
; means a number of steps proportional to n.
; If we choose to check the right branch first, we'd immediately find it,
; which mean a constant number of steps.
