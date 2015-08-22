; This exercise asks us to define a procedure `last-pair` that returns the
; list that contains only the last element of a given (nonempty) list.

; The strategy for fetching the last pair is to `cdr`-down the list until
; `(cdr list)` is `null`, at which point we return `(car list)`

(define (last-pair lst)
  (if (null? (cdr lst))
    (car lst)
    (last-pair (cdr lst))))

; Testing:
(last-pair (list 23 72 149 34)) ; => Returns 34
