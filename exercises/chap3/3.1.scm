; Local variables - creating an accumulator

(define (make-accumulator init-value)
  (lambda (summand)
    (begin (set! init-value (+ init-value summand)))
           init-value))

(define A (make-accumulator 10))

(A 0)
; => 10

(A 10)
; => 20

(A 30)
; => 50
