; Let's compare both implementations of `expmod`:

; Original implementation
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

; Alyssa P. Hacker's implementation
(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

; Where `fast-expt` is defined as:
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))


; Let's call the original `expmod` procedure on an arbitraty input to get
; a feeling about what's going on:

; (expmod 3 4 3)
; (remainder (square (expmod 3 2 3)) 3)
; (remainder (square (remainder (* 3 (expmod 3 1 3)) 3)) 3)
; (remainder (square (remainder (* 3 (remainder (* 3 (expmod 3 0 3)) 3)) 3)) 3)
; (remainder (square (remainder (* 3 (remainder (* 3 1) 3)) 3)) 3)
; (remainder (square (remainder (* 3 (remainder 3 3)) 3)) 3)
; (remainder (square (remainder (* 3 0) 3)) 3)
; (remainder (square (remainder 0 3)) 3)
; (remainder (square 0) 3)
; 0

; Calling Hacker's `expmod`:
; (expmod 3 4 3)
; (remainder (fast-exp 3 4) 3)
; (remainder (square (fast-exp 3 2)) 3)
; (remainder (square (square (fast-exp 3 1))) 3)
; (remainder (square (square (* 3 (fast-exp 3 0)))) 3)
; (remainder (square (square (* 3 1))) 3)
; (remainder (square (square 3)) 3)
; (remainder (square 9) 3)
; (remainder 81 3)
; 0

; We can see the difference in the execution of the two procedures.
; The first one has the advantage of squaring smaller numbers, since
; is only squares the remainder of the previous call.
