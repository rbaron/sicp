; This exercise asks us to account and signal an error for the case
; in which the divisor interval spans zero

; Helper functions
(define (make-interval a b) (cons a b))

(define (upper-bound interval) (cdr interval))

(define (lower-bound interval) (car interval))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

; Fixing `div-interval`:
(define (div-interval x y)
  (if (and (< (lower-bound y) 0) (> (upper-bound y) 0))
    (error "Interval spans zero. Cannot divide!")
    (mul-interval x
      (make-interval (/ 1.0 (upper-bound y))
                     (/ 1.0 (lower-bound y))))))

; Trying it out:
(div-interval (make-interval 1 2) (make-interval -1 2))
; => Should signal error

(div-interval (make-interval 1 2) (make-interval 2 6))
; => Should produce (.16666, 1.)
