; Applicative-order definition of unless.

(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

(define (factorial n)
  (unless (= n 1)
          (* n (factorial (- n 1)))
          1))

; Now, when `factorial` is called, the `unless` expression will
; have all its arguments evaluated. This will cause an uncondi-
; tional call to `factorial`, which in turn will cause an un-
; conditional recursive call to `factorial`... The progam will
; hang.

; See it for yourself! Uncomment the following line:
; (factorial 10)
; => Aborting!: maximum recursion depth exceeded

; With normal-order evaluation, the recursive call would be
; delayed until - and if - it is needed. The program would
; work just fine.
