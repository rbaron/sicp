; This exercise asks us to rewrite an optimized version of `mul-interval`.
; It suggests that we can check for the signs of the endpoins of the intervals
; and break it into 9 cases, only one of which requires more than two multiplications.

; Given helper functions

(define (make-interval a b) (cons a b))

(define (upper-bound interval) (cdr interval))

(define (lower-bound interval) (car interval))

; Original `mul-interval` procedure
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

; There are 3 possible "states" an interval can be regarding the sign of
; its endpoints. They are:
;
; Full-left  (state 0): Both signs of its endpoints are negative (interval is fully left from 0-point)
; Cross-zero (state 1): Signs of the interval's endpoints are different (interval crosses 0-point)
; Full-right (state 2): Both signs of its endpoints are positive (interval is fully right from 0-point)

; Analysing two intervals at once, we arrive at 3*3 = 9 possible states:

;Interval | INT1 | INT2 |   Multiplication results
;State    |0|1|2|||0|1|2|
;         ---------------
;         |x| | | |x| | |   (1b*2b, 1a*2a)
;         |x| | | | |x| |   (1a*2b, 1a*2a)
;         |x| | | | | |x|   (1a*2b, 1b*2a)
;         | |x| | |x| | |   (1b*2a, 1a*2a)
;         | |x| | | |x| |   (min(1a*2b, 1b*2a), max(1a*2a, 1b*2b))
;         | |x| | | | |x|   (1a*2b, 1b*2b)
;         | | |x| |x| | |   (1b*2a, 1a*2b)
;         | | |x| | |x| |   (1b*2a, 1b*2b)
;         | | |x| | | |x|   (1a*2a, 1b*2b)

; Let's implement a procedure that extracts the current state of an
; interval and use that to switch our conditionals
(define (state int)
  (cond ((and (< (lower-bound int) 0) (< (upper-bound int) 0)) 0)
        ((and (< (lower-bound int) 0) (> (upper-bound int) 0)) 1)
        ((and (> (lower-bound int) 0) (> (upper-bound int) 0)) 2)))

; And, finally, our optimized version of the multiplication procedure between two intervals:
(define (mul-interval-opt x y)
  (cond
    ((and (= (state x) 0) (= (state y) 0))
      (make-interval (* (upper-bound x) (upper-bound y))
                     (* (lower-bound x) (lower-bound y))))
    ((and (= (state x) 0) (= (state y) 1))
      (make-interval (* (lower-bound x) (upper-bound y))
                     (* (lower-bound x) (lower-bound y))))
    ((and (= (state x) 0) (= (state y) 2))
      (make-interval (* (lower-bound x) (upper-bound y))
                     (* (upper-bound x) (lower-bound y))))
    ((and (= (state x) 1) (= (state y) 0))
      (make-interval (* (upper-bound x) (lower-bound y))
                     (* (lower-bound x) (lower-bound y))))
    ((and (= (state x) 1) (= (state y) 1))
      (make-interval (min (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y)))
                     (max (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y)))))
    ((and (= (state x) 1) (= (state y) 2))
      (make-interval (* (lower-bound x) (upper-bound y))
                     (* (upper-bound x) (upper-bound y))))
    ((and (= (state x) 2) (= (state y) 0))
      (make-interval (* (upper-bound x) (lower-bound y))
                     (* (lower-bound x) (upper-bound y))))
    ((and (= (state x) 2) (= (state y) 1))
      (make-interval (* (upper-bound x) (lower-bound y))
                     (* (upper-bound x) (upper-bound y))))
    ((and (= (state x) 2) (= (state y) 2))
      (make-interval (* (lower-bound x) (lower-bound y))
                     (* (upper-bound x) (upper-bound y))))
    ))

; Tests
(define (try-ints x y)
  (ints-equal?
    (mul-interval-opt x y)
    (mul-interval x y)))

(define (ints-equal? a b)
  (cond ((and (= (lower-bound a) (lower-bound b))
             (= (upper-bound a) (upper-bound b)))
        (newline)
        (display "Intervals are equal!"))
      (else (newline)
            (display "******* Intervals are _NOT_ equal! ********")
            (newline)
            (display a)
            (display b))))


; State 0 0
(try-ints (make-interval -2 -1) (make-interval -5 -3))

; State 0 1
(try-ints (make-interval -2 -1) (make-interval -3 5))

; State 0 2
(try-ints (make-interval -2 -1) (make-interval 3 5))

; State 1 0
(try-ints (make-interval -2 1) (make-interval -5 -3))

; State 1 1; case 0
(try-ints (make-interval -10 10) (make-interval -5 3))
; State 1 1; case 1
(try-ints (make-interval -10 10) (make-interval -1 3))

; State 1 2
(try-ints (make-interval -10 10) (make-interval 3 5))

; State 2 0
(try-ints (make-interval 1 2) (make-interval -5 -3))

; State 2 1
(try-ints (make-interval 1 2) (make-interval -3 5))

; State 2 2
(try-ints (make-interval 1 2) (make-interval 3 5))
