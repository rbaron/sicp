; This exercise asks us to write a procedure with an arbitrary number of arguments.
; The procedure should receive an integer `a`and a list of integers and return a list
; of the integers, among the arguments, that have the same parity as the `a`.

(define (single-same-parity a b)
  (= (remainder a 2)
     (remainder b 2)))


(define (same-parity a . lst)
  (if (null? lst)
    (cons a ())
    (if (single-same-parity a (car lst))
       (cons a (apply same-parity lst))
       (apply same-parity (cons a (cdr lst))))))

(same-parity 1 2 3 4 5 6 7)


; There are two nice things to notice in this implementation:
; 1. We can leverage the fact that if `a` and `b` have the same
;   parity, we can test `c` against `b` in the next iteration, instead
;   of having to pass `a` as an argument. This makes the recursion nicer.
; 2. We can "unpack" a list to be used as arguments on a procedure that
;   accepts an arbitrary number of arguments by using the procedure `apply`.
;   Its syntax is:
;
;   `(apply procedure argument-list)`
