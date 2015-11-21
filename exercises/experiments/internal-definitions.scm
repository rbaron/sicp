; Let's analyze how internal procedures are evaluated

; Example from exercise 4.19:

(let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))
; => Premature reference to reserved name: a

; That's weird. Let's change the order of the internal
; defines:

(let ((a 1))
  (define (f x)
    (define a 5)
    (define b (+ a x))
    (+ a b))
  (f 10))
; => 20
; It now works as expected.
