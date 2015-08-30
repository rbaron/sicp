; This exercise asks us to implement the `reverse` procedure using both
; `fold-left` and `fold-right`.

; 1. Using `fold-right`:
(define (reverse sequence)
  (fold-right (lambda (el acc) (append acc (list el))) () sequence))

; Testing:
(reverse (list 1 2 3 4))
; => (4 3 2 1)

; 1. Using `fold-left`:
(define (reverse sequence)
  (fold-left (lambda (el acc) (cons el acc) () sequence))

; Testing:
(reverse (list 1 2 3 4))
; => (4 3 2 1)
