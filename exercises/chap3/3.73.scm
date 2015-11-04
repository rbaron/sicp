; Modeling RC circuits

; The excercise asks us to write a `RC` procedure
; that takes as input the values of R, C and dt and
; returns a procedure that take as input a stream
; of current values and the initial capacitor
; voltage v0

(define (RC R C dt)

  (define (process i-stream v0)
    (add-streams (scale-stream i-stream R)
                 (integral (scale-stream i-stream (/ 1 C))
                           v0
                           dt)))

  process)


; Where `integral` is a given procedure:
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)
