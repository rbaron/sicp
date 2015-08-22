; This exercise asks us to investigate why the two procedures for
; calculating the equivalent resistor of two resistors in parallel
; produce different results, even though they are algebraically equivalent.

; Given procedures (including the ones implemented on 2.12):
(define (make-interval a b) (cons a b))

(define (upper-bound interval) (cdr interval))

(define (lower-bound interval) (car interval))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent midpoint tol)
  (make-center-width midpoint (* midpoint tol)))

(define (percent int)
  (/ (width int) (center int)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

; Two ways of calculating the equivalent of two paralell resistors:
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

; Let's try out some values to get the feeling of what's going on
(define (show-int int)
  (newline)
  (display (center int))
  (display " +- ")
  (display (percent int)))
  (newline)

(define r1 (make-center-percent 1 .01))

(show-int (par1 r1 r1)) ; => Displays .5002000200020003 +- .02999200239928024
(show-int (par2 r1 r1)) ; => Displays .5 +- 1.0000000000000009e-2

; We can see that the complaint makes sense. Lem is right.
