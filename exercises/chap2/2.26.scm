; In this exercise we are supposed to simply evaluate the following expressions:

(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
; => (1 2 3 4 5 6)

(cons x y)
; ((1 2 3) 4 5 6)
; This is unexpected. `cons` accept a list as the second argument!
; We can thing of `cons` as  scala's :: operator. It appends an element
; to the front of the second argument!

(list x y)
; ((1 2 3) (4 5 6))
; This works as expected. It generates a new lists which elements are
; the two arguments. Very straight forward.
