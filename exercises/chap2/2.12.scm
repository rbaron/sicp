; This exercise asks us to implement a `make-center-percent`
; constructor, which should receive a midpoint value and a
; tolerance and produces the desired interval.

; Given procedures:

(define (make-interval a b) (cons a b))

(define (upper-bound interval) (cdr interval))

(define (lower-bound interval) (car interval))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

; Solution (assument tol is already divided by 100)
(define (make-center-percent midpoint tol)
  (make-center-width midpoint (* midpoint tol)))

(define (percent int)
  (/ (width int) (center int)))


; Testing
(make-center-percent 5 .1)
; => (4.5, 5.5)

(percent (make-center-percent 5 .1))
; => .1
