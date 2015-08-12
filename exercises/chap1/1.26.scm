; The new implementation for `expmod`:

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
          (remainder (* (expmod base (/ exp 2) m)
                        (expmod base (/ exp 2) m))
                     m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

; By recursing twice, Louis transformed a linear recursive procedure with `log(n)` steps into a
; binary-tree recursive procedure with height `h ~ log(n)`. For such tree recursion there are
; `~ 2^(h+1)-1` states (many are repeated). Writing the number of steps as a function of `n` yields:

; `n_steps ~ 2^(log(n) + 1) - 1 ~ n`

; Which makes it a procedure that takes an amount of steps proportinal to `n`.
