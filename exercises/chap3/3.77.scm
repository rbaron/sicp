; Alternative, explicit version of `integral`

; The version of `integral` we have been using so far
; was implemented using "implicit" stream definition:

; (define (integral integrand initial-value dt)
;   (define int
;     (cons-stream initial-value
;                  (add-streams (scale-stream integrand dt)
;                               int)))

; That is, the inner `int` procedure references itself
; via the second argument of `add-streams`. This is only
; possible because `cons-stream` has the characteristic
; of _delaying_ the evaluation of it's second argument,
; otherwise we would end up with an infinite loop.

; The book goes a little further and exploit how we could
; use _delayed arguments_ withing `integral`:

; (define (integral delayed-integrand initial-value dt)
;   (define int
;     (cons-stream initial-value
;                  (let ((integrand (force delayed-integrand)))
;                    (add-streams (scale-stream integrand dt)
;                                 int))))
;   int)

; This allows us to delay the evaluation of `delayed-integrand`
; until the point the second argument of `cons-stream` is evaluated,
; which allows us to use constructions such as:

; (define (solve f y0 dt)
;   (define y (integral (delay dy) y0 dt))
;   (define dy (stream-map f y))
;   y)

; which otherwise wouldn't be possible, since `y` depends on
; `dy`, which is not defined when `y` is evaluated.

; The exercise proposes an altenative version of the procedure,
; which does not rely on such implicit form:

; (define (integral integrand initial-value dt)
;   (cons-stream initial-value
;                (if (stream-null? integrand)
;                    the-empty-stream
;                    (integral (stream-cdr integrand)
;                              (+ (* dt (stream-car integrand))
;                                 initial-value)
;                              dt))))

; How would we go about to make sure this new version also allows
; delayed arguments in order to be able to handle constructs such
; as the above `solve` procedure?

; => Following the first implementation, we can use `delay` to
; postpone the evaluation of `integrand` until it's needed
; on the second argument of `cons-stream`:

(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral (delay (stream-cdr integrand))
                               (+ (* dt (stream-car integrand))
                                  initial-value)
                               dt)))))

; Let's try it out:

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

; Let's try to solve dy/dt = f(y) for f(y) = y, which f(x) = e^x
; is a solution with y0 = 1

(define f (lambda (x) x))
(define solution-stream (solve f 1 .001))

(stream-ref solution-stream 1000)
; => 2.7169 ~ f(1) = e^1
