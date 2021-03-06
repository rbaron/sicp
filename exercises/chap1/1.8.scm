(define (improve guess x)
  (/
    (+
     (* 2 guess)
     (/ x (square guess)))
    3))

(define (cube x) (* x x x))

(define (square x) (* x x))

(define (good-enough? guess x)
  (display "Current guess: ")
  (display (+ guess 0.0))(newline)
  (display "Current guess cubed: ")
  (display (+ (cube guess) 0.0))(newline)
  (display "x: ")
  (display (+ x 0.0))(newline)
  (display "Abs difference: ")
  (display (+ (abs (- (cube guess) x)) 0.0))(newline)
  (display "============")(newline)
  (< (abs (- (cube guess) x)) 0.001))

(define (cube-root-iter guess x)
  (if (good-enough? guess x)
    (+ guess 0.0)
    (cube-root-iter (improve guess x)
      x)))

(define (cube-root x) (cube-root-iter 1.0 x))


; Sample output
; =============
;
; > (cube-root -512)
;   ...
;   ;Value: -8.000000025890593
;
;
; > (cube-root 8)
;   ...
;   ;Value: 2.000004911675504
