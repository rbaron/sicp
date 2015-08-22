; This exercise asks us to define a procedure `reverse` that takes a list as
; argument and returns a list of the same elements in reverse order

; A high inefficient way of achieving this (O(n^2)) would be:

(define (last-pair lst)
  (if (null? (cdr lst))
    (car lst)
    (last-pair (cdr lst))))

(define (all-but-last lst)
  (if (null? (cdr lst))
    ()
    (cons (car lst) (all-but-last (cdr lst)))))

(define (rev lst)
  (if (null? lst)
    ()
    (cons (last-pair lst) (rev (all-but-last lst)))))

(rev (list 1 2 3 4)) ; => Produces (4 3 2 1)

; A smarter way would be to make a recursive `rev2` that appends the
; last element of the list to the result of reversing the `cdr` of it:

(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))

(define (rev2 lst)
  (if (null? lst)
    ()
    (append (rev2 (cdr lst))  (cons (car lst) ()))))

(rev2 (list 1 2 3 4)) ; => Produces (4 3 2 1)
