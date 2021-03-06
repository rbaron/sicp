; We can use a print statement to investigate the iteration process.

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (display "Current guess: ")
  (display (+ guess 0.0))(newline)
  (display "Current guess squared: ")
  (display (+ (square guess) 0.0))(newline)
  (display "x: ")
  (display (+ x 0.0))(newline)
  (display "Abs difference: ")
  (display (+ (abs (- (square guess) x)) 0.0))(newline)
  (display "============")(newline)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    (+ guess 0.0)
    (sqrt-iter (improve guess x)
      x)))

; Calling (sqrt-iter 1 2) yields:
;
; > (sqrt-iter 1 2)
;   Current guess: 1.
;   Current guess squared: 1.
;   x: 2.
;   Abs difference: 1.
;   ============
;   Current guess: 1.5
;   Current guess squared: 2.25
;   x: 2.
;   Abs difference: .25
;   ============
;   Current guess: 1.4166666666666667
;   Current guess squared: 2.0069444444444446
;   x: 2.
;   Abs difference: 6.944444444444444e-3
;   ============
;   Current guess: 1.4142156862745099
;   Current guess squared: 2.000006007304883
;   x: 2.
;   Abs difference: 6.007304882737409e-6
;   ============
;   ;Value: 1.4142156862745099
;
;
; Analysis for a small value
; ==========================
;
; Calling (sqrt-iter 1 0.0000001) yields:
;
; > (sqrt-iter 1 0.0000001)
;   Current guess: 1.
;   Current guess squared: 1.
;   x: .0000001
;   Abs difference: .9999999
;   ============
;   Current guess: .50000005
;   Current guess squared: .2500000500000025
;   x: .0000001
;   Abs difference: .24999995000000252
;   ============
;   Current guess: .25000012499999
;   Current guess squared: .06250006250001064
;   x: .0000001
;   Abs difference: 6.2499962500010636e-2
;   ============
;   Current guess: .12500026249989502
;   Current guess squared: .01562506562504266
;   x: .0000001
;   Abs difference: 1.5624965625042661e-2
;   ============
;   Current guess: .06250053124910751
;   Current guess squared: 3.9063164064206644e-3
;   x: .0000001
;   Abs difference: 3.9062164064206646e-3
;   ============
;   Current guess: .03125106561775382
;   Current guess squared: 9.76629102245155e-4
;   x: .0000001
;   Abs difference: 9.76529102245155e-4
;   ============
;   ;Value: .03125106561775382
;
;
; It doesn't work as expected because we are trying to find the square root of a number that is
; much smaller than the tolerance of 0.001. Any `guess` which square is smaller than the tolerance
; will be accepted. This is clearly wrong.
;
; Eg.:
;
; > (abs (- (square 0.01) 0.00000000001)) => .00009999999
;
;
; Analysis for a big value
; =========================
;
; Since arithmetic is done within a given precision, least significant digits can be omitted.
;
; Eg.:
;
; (- 0.1 10000000000000000) => -10000000000000000
;
; Also:
;
; > (sqrt-iter 1 1e60)
;   Current guess: 1.
;   Current guess squared: 1.
;   x: 1e60
;   Abs difference: 1e60
;   ============
;   Current guess: 5e59
;   Current guess squared: 2.4999999999999996e119
;   x: 1e60
;   Abs difference: 2.4999999999999996e119
;   ============
;   Current guess: 2.5e59
;   Current guess squared: 6.249999999999999e118
;   x: 1e60
;   Abs difference: 6.249999999999999e118
;   ============
;   Current guess: 1.25e59
;   Current guess squared: 1.5624999999999997e118
;   x: 1e60
;   Abs difference: 1.5624999999999997e118
;   ============
;   Current guess: 6.25e58
;   Current guess squared: 3.9062499999999994e117
;   x: 1e60
;   Abs difference: 3.9062499999999994e117
;   ============
;
;   ........
;
;
;   ============
;   Current guess: 1e30
;   Current guess squared: 1.0000000000000001e60
;   x: 1e60
;   Abs difference: 1.78405961588245e44
;   ============
;   Current guess: 1e30
;   Current guess squared: 1.0000000000000001e60
;   x: 1e60
;   Abs difference: 1.78405961588245e44
;   ============
;
;   ...
;
;   Hangs.
;
;
; The reason it hangs is that, inside `average`, we are suming a two big numbers. One of them is
; slightly less than the other, and that difference is neglected. Specifically, what's happening is:
;
; > (average 1e30 (/ 1e60 1e30))
;   => (average 1e30 9.999999999999999e29)
;   => (/ (+ 1e30 9.999999999999999e29) 2))
;   => (/ 2e30 2)
;   => 1e30
;
;   ;Value: 1e30
;
;
; > (- 1e70 (* 1e35 1e35))
;   => ;Value: 1.532495540865889e54
;
;
; Fixing the algorithm
; ====================
;
; The idea is considering the relative improvement instead of absolute improvement.
; This is done by stopping the algorithm once `abs(guess^2 - x)/x < 0.001`
;
; (define (good-enough? guess x)
;   (< (/ (abs (- (square guess) x)) x) 0.001))
;
; > (sqrt-iter 1 1e60)
;
;   ;Value: 1.0000788456669446e30
;
; > (sqrt-iter 1 0.00000001)
;
;   ;Value: 1.0000040611237676e-4
