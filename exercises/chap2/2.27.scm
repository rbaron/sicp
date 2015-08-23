; This exercise asks us to create a `deep-reverse` procedure based on exercise 2.18.
; It should reverse the elements of the given lists and of its sublists, recursively.

(define (reverse lst)
  (if (null? lst)
    lst
    (append (reverse (cdr lst)) (cons (car lst) ()))))

; We can think of `deep-reverse` as a procedure that calls `reverse` on the
; result of reversing the children. This idea maps nicely to the following procedure:
(define (deep-reverse lst)
  (if (not (pair? lst))
    lst
    (reverse (map deep-reverse lst))))

(define x (list (list 1 2) (list 3 4)))
(deep-reverse x)
; => ((4 3) (2 1))

(define x (list (list 1 2 3) (list 3 4)))
(deep-reverse x)
; => ((4 3) (3 2 1))

; The last example shows that it works on trees with more than two branches! :)
