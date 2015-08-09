(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

; Given procedure to print points
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (print-seg seg)
  (newline)
  (print-point (start-segment seg))
  (print-point (end-segment seg)))


; Let's design our `midpoint-segment` procedure. It should average the x and y
; coordinates of a pair of points and make a new point with those
(define (midpoint-segment seg)
  (make-point
    (avg
      (x-point (start-segment seg))
      (x-point (end-segment seg)))
    (avg
      (y-point (start-segment seg))
      (y-point (end-segment seg)))))

(define (avg a b) (/ (+ a b) 2))

; Let's try it out
(define (p1) (make-point 4 -2))
(define (p2) (make-point -3 8))
(define (s1) (make-segment (p1) (p2)))

(print-point (midpoint-segment (s1)))
