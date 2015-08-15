; Let's use our solution to 2.7:

(define (make-interval a b) (cons a b))
(define (upper-bound interval) (cdr interval))
(define (lower-bound interval) (car interval))

; To implement `sub-interval`, we can imagine the bounds of
; the resulting interval to be the largest possible, since it's the
; most 'safe' range.
(define (sub-interval i1 i2)
  (make-interval
    (- (lower-bound i1) (upper-bound i2))
    (- (upper-bound i1) (lower-bound i2))))

; Trying it out:
(sub-interval
  (make-interval 4 10)
  (make-interval 2 8))

; => (-4 . 8)

; That is, the resulting value will be in the range [-4,  8], which is the
; biggest possible range, or the 'safest' one.
