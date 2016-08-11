; Iterative improving abstraction

; Load this file with
; $ cat 1.46.scm - | mit-scheme

(define (iterative-improve good-enough? improve)
  (define (inner guess)
    (if (good-enough? guess)
      guess
      (inner (improve guess))))
  inner)

; Let's rewrite sqrt in terms of iterative-improve:

(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) .0001))

  (define (improve guess)
    (average guess (/ x guess)))

  ((iterative-improve good-enough? improve) 1.))

; Testing

(sqrt 2)
; => 1.4142156862745097

; And fixed-point:

(define (fixed-point f first-guess)
  (define (good-enough? guess)
    (< (abs (- (f guess) guess)) .0001))

  ; Notice how the improve procedure is simple the f procedure,
  ; since in order to find a fixed point we keep re-applying f
  ; on successive guesses
  ((iterative-improve good-enough? f) first-guess))

; Testing

(fixed-point cos 1.0)
; => .7391301765296711
