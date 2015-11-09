; Solving specific second order differential equations

; We learned that we can represent loops in signal processing
; systems by _delaying_ the execution of some arguments in order
; to avoid using procedures that are not defined yet.

; The version of `integral` that supports the delayed evaluation
; of the integrand is:

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

; Let's try to model the given second order equation:

; d²y/dt² - a*dy/dt - by = 0

; Since we are interested in finding `y` given derivatives
; up to the second order, our system will have two integrators:

; 1. Integrating d²y/dt² yields dy/dy
; 2. Integrating dy/dt yields y

; These two integrated streams are linked together trough the
; remodelling of the differential equation:

; d²y/dt² = a*dy/dt + by

; Our loop is then closed.

; Let's write this as a procedure:

(define (solve-2nd a b dt y0 dy0)
  (define dy (integral (delay ddy) dy0 dt))
  (define y (integral (delay dy) y0 dt))
  (define ddy (add-streams (scale-stream dy a)
                           (scale-stream y b)))
  y)

(define (scale-stream stream factor)
  (stream-map (lambda (e) (* e factor)) stream))

; Let's test it:

(define (add-streams s1 s2)
  (stream-map + s1 s2))


(define solution-stream (solve-2nd 1 1 .001 2 0))

(stream-ref solution-stream 1000)
; => 3.5640540136744363
