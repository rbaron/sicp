; Given helper functions
(define (print-rat x)
   (newline)
   (display (numer x))
   (display "/")
   (display (denom x)))

(define (gcd a b)
  (if (= 0 b)
    a
    (gcd b (remainder a b))))

(define (numer x) (car x))

(define (denom x) (cdr x))

; Let's modify make-rat into make-rat-norm so that it normalized
; the sign between the numerator and denominator

(define (same-sign? a b)
  (or
    (and (< a 0) (< b 0))
    (and (>= a 0) (>= b 0))))

(define (make-rat-abs n d have-same-sign)
  (let ((g (gcd n d)))
    (if have-same-sign
      (cons (/ n g) (/ d g))
      (cons (* (/ n g) -1) (/ d g)))))

(define (make-rat-norm n d)
  (make-rat-abs (abs n) (abs d) (same-sign? n d)))

; Testcases
(print-rat (make-rat-norm 5 4))   ;  5/4
(print-rat (make-rat-norm -5 4))  ; -5/4
(print-rat (make-rat-norm 5 -4))  ; -5/4
(print-rat (make-rat-norm -5 -4)) ;  5/4


; After I implemented this solution, I found a nicer one online
; (at http://www.billthelizard.com/2010/09/sicp-21-rational-numbers.html).
; We could only look for the sign of the denominator, and if it's
; nevative, we multiply both `n` and `d` by -1, as follows:

(define (make-rat-nicer n d)
  (let ((g (gcd n d)))
    (if (< d 0)
      (cons (/ (* n -1) g) (/ (* d -1) g))
      (cons (/ n g) (/ d g)))))

; Testcases
(print-rat (make-rat-nicer 5 4))
(print-rat (make-rat-nicer -5 4))
(print-rat (make-rat-nicer 5 -4))
(print-rat (make-rat-nicer -5 -4))

; This isn't the expected result. The reason is that our `gcd` procedure
; doesn't work very well for negative numbers. E.g.:
; (gcd -4 12) = -4, which is wrong, since it should be 4.
; If we simply comment out our `gcd` procedure, `make-rat-nicer` will use the
; builtin, correct-for-negative-numbers `gcd` algorithm and will yield
; the correct results. Another option would be to correct our `gcd` prodecure:

(define (gcd2 a b)
  (if (= b 0)
    (abs a)
    (gcd2 b (remainder a b))))

(define (make-rat-nicer2 n d)
  (let ((g (gcd2 n d)))
    (if (< d 0)
      (cons (/ (* n -1) g) (/ (* d -1) g))
      (cons (/ n g) (/ d g)))))

; Testcases
(print-rat (make-rat-nicer2 5 4))
(print-rat (make-rat-nicer2 -5 4))
(print-rat (make-rat-nicer2 5 -4))
(print-rat (make-rat-nicer2 -5 -4))
