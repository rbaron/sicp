; The problem with defining `if` in terms of `cond` is that, since Scheme uses applicative-order
; execution, the arguments of the function `new-if` will be evaluated either before being plugged
; into the function.
;
; In this specific case, the `else-clause` will be evaluated even when the predicate `good-enough`
; evaluates to false. Since the `else-clause` consists of a recursive call, the program will loop
; forever.
;
; Here's an attempt to run it:

(define (new-if predicate then-clause else-clause)
   (cond (predicate then-clause)
            (else else-clause)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
      x)))

; (sqrt-iter 1 2)
;
; => Hangs

; If we choose to use the special-form `if`, we get:

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
      x)))

(sqrt-iter 1 2)
; => 577/408 => 1.4142156862745099
