; This exercise asks us to write a `fringe` prodecure that collects the leaves
; from a tree into a list.

(define (fringe lst)
  (cond
    ((null? lst)
      lst)
    ((not (pair? lst))
      (list lst))
    (else
      (append (fringe (car lst))
              (fringe (cdr lst))))))

(define x (list (list 1 2) (list 3 4)))

(fringe x)
; => (1 2 3 4)

(fringe (list x x))
; => (1 2 3 4 1 2 3 4)

; Recursive reasoning: at each node, we append the two lists of
; leaves that came from each children.

; Enlightment!

; Take for instance the following list:
(list 1 2 3)

; Even thought we might be drawn to imagine the following list as a root node
; with tree children, we can actually reason about it as a binary tree! Since
; that is equivalent to:

(cons 1 (cons 2 (cons 3 ())))

; We can draw this as a binary tree!

;          .
;        /   \
;       1     .
;           /   \
;          2     .
;              /   \
;            3       nil

; By using this reasing, we always have to handle at most 2 children,
; which allows us to use `append "call left" "call right"`, just like we
; did with:

;   (append (fringe (car lst))
;           (fringe (cdr lst))))))

; Cool!
