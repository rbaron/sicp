; This exercise asks us to complete two procedures that calculate the
; list of the squared elements of a list:

(define (square-list items)
  (if (null? items)
      items
      (cons (* (car items) (car items)) (square-list (cdr items)))))

(square-list (list 1 2 3 4 5))
; => (1 4 9 16 25)

(define (square-list items)
  (map (lambda (x) (* x x)) items))

(square-list (list 1 2 3 4 5))
; => (1 4 9 16 25)
