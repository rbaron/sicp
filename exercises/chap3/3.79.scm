; Generalizing `solve-2nd` for arbitrary functions

; The idea is to compute dy/dy and y from d²t/dt² and
; feed it to a funciton `f`, passed as argument to `solve-2nd`.
; The output from f is then fed back to d²t/dt².

(define (solve-2nd f dt y0 dy0)
  (define dy (integral (delay (f dy y)) dy0 dt))
  (define y (integral (delay dy) y0 dt))
  y)


; ========================================
;    Begin copied code for testing
; ========================================
(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

(define (scale-stream stream factor)
  (stream-map (lambda (e) (* e factor)) stream))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (f dy y)
  (add-streams (scale-stream dy 1.)
               (scale-stream y 1.)))

; ========================================
;    END OF copied code for testing
; ========================================

(define solution-stream (solve-2nd f .001 2 0))

(stream-ref solution-stream 1000)
; => 3.5640540136744363
; (same results as exercise 3.78!)
