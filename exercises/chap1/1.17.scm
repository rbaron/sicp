; In order to avoid any conflics, I'm gonna use `m` to denote the
; multiplication procedure.


; Let's look at some intuitive cases for `a*b`:
; 1. `b` is even
  ; => a*6 = (a*2)*3 = (double a)*(halve b)
; 2. `b` is odd
; => a*5 = a + a*4
;
; Those equations are sufficient for us to implement the required procedure.


; Available procedures
(define (double x) (* x 2))

(define (halve x) (/ x 2))

(define (even? n)
   (= (remainder n 2) 0))

; Logarithmic linear recursive multiplication
(define (m a b)
  (if (= b 0)
    0
    (if (even? b)
      (m (double a) (halve b))
      (+ a (m a (- b 1))))))

(m 4 5)
; => 20

(m 3 4)
; => 12
