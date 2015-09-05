; In this exercise, we are asked to develop an abstraction for `segment`.

; Constructor
(define (make-segment from-vect to-vect)
  (cons from-vect to-vect))

; Selectors
(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))
