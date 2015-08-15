; Let's implement `make-interval`, `upper-bound` and `lower-bound`:

; We're gonna go ahead and assume `a` is always less then `b`
(define (make-interval a b) (cons a b))

(define (upper-bound interval) (cdr interval))
(define (lower-bound interval) (car interval))

; Trying it out:
(upper-bound (make-interval 2 10))
; => 10

(lower-bound (make-interval 2 10))
; => 2
