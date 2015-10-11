; Let's show that, once we lose referencial transparency, different
; orders of evaulation may yield different results.

; The exercise asks us to come up with a `f` procedure, so that the
; expression:

; (+ (f 0) (f 1))

; Will be evaluated differently, depending whether the first argument
; or the second argument is evaluated first.

; The idea is to leverage inner states to come up with a `f` that behaves
; in such way.

(define f
  (let ((state 0))
    (lambda (arg)
      (if (or (> state 0) (> arg 0))
        (begin (set! state (+ 1 state))
               state)
        state))))

; Let's evaluate left argument first

; => (+ (f 0) (f 1))
; => (+ 0 (f 1)) ; => Side effect: keep state to 0
; => (+ 0 1) ;> Side effect: set state to 1
; => 1

; Now the right argument first
; => (+ (f 0) (f 1))
; => (+ (f 0) 1) ; => Side effect: set state to 1
; => (+ 2 1) ;> Side effect: set state to 1
; => 3

; Let's try to identify the order which the arguments are actually
; evaluated.
(+ (f 0) (f 1))
; => 3
; This suggests arguments are evaluated from right to left.

; Quick and dirty way of resetting the internal state to 0. Sorry.
(define f
  (let ((state 0))
    (lambda (arg)
      (if (or (> state 0) (> arg 0))
        (begin (set! state (+ 1 state))
               state)
        state))))


(+ (f 1) (f 0))
; => 1
; This confirms our suspicion that the arguments are evaluated form right
; to left.
