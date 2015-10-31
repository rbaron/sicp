; Analyzing Louis' alternative version of `sqrt-stream`

; Louis says we didn't have to define a internal `guesses`
; stream. His version:

(define (sqrt-stream x)
  (cons-stream 1.0
               (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                           (sqrt-stream x))))

; And our previous implementation:

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

; Why is Louis' version less efficient?

; => Louis' version is less efficient because a new
; stream is created on every step. Let's reason about
; it. Let `imp()` represent a call to `sqrt-improve`.
; We would have `(sqrt-stream 2)` return a stream as:

; 1.0
; imp(1.0)
; imp(imp(1.0))
; imp(imp(imp(1.0)))
; ...

; The original version would produce the following stream:

; 1.0
; imp(1.0) = v1
; imp(v1) = v2
; imp(v2) = v3
; ...


; Would the two versions still differ if `delay` was
; implemented without memoization?

; Without memoization, the two versions would be
; equivalent.
