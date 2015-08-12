(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
    angle
    (p (sine (/ angle 3.0)))))


; a. How many times is the procedure `p` called when calling (sine 12.15)?
;
; Intuitively, `p` will be called whenever `sine` is called with an argument
; `angle` > 0.1. The call stack will look like:
;
; (sine 12.15)
; (p (sine 4.05))
; (p (p (sine 1.35)))
; (p (p (p (sine .45))))
; (p (p (p (p (sine .15)))))
; (p (p (p (p (p (sine .05))))))
; (p (p (p (p (p 0.05)))))
; (p (p (p (p .1495))))
; (p (p (p .4351)))
; (p (p .9758))
; (p -.7891)
; -.4018
;
; Thus, `p` is called 5 times.
;
; Using python's `math.sin`, we'd get:
;
; $ python -c "import math; print math.sin(12.15)"
; -0.404443822849
;
; b. The number of steps is proportinal to how many times we must divide the input `n` by 3
; until it is smaller than 0.1. Let `s` be the number of steps:
;
;   n/3^s = .1
;   s = log_3 .1n
;
; Thus the number of steps grows logarithmicaly with `n`. Since, as we can see on the simulated
; call stack above, the space growth is the exact same as the number of steps (since the stack
; grows by one with every call to `p`), the space growth is also logarithmic.
