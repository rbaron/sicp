; In the chapter about streams, we are introduced to a way of
; intergrating a function represented by a stream. It works like:

(define (integral initial-value integrand dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

; Helpers:
(define (scale-stream stream factor)
  (stream-map (lambda (e) (* e factor))
              stream))

(define (add-streams s1 s2)
  (if (stream-null? s1)
      the-empty-stream
      (cons-stream (+ (stream-car s1) (stream-car s2))
                   (add-streams (stream-cdr s1) (stream-cdr s2)))))


; Let's try to integrate f(x) = x from 0 to 1:

(define f (lambda (x) x))

; Let's produce a stream that is the value of mapping f
; to values of x from 0 to 1, taking steps of .01:

(define (make-stream from to step)
  (if (> from to)
    the-empty-stream
    (cons-stream from
                 (make-stream (+ from step) to step))))

(define x-stream (make-stream 0 1 .01))

(define f-stream (stream-map f x-stream))

(define f-integral-stream (integral 0 f-stream .01))

(stream-ref f-integral-stream 100)
; => .4950000000000004

; Cool! That's really close to what we'd expect (area = .5)

; Let's try with cos(x):
(define cos-stream (stream-map cos x-stream))
(define cos-integral-stream (integral 0 cos-stream .01))
(stream-ref cos-integral-stream 100)
; => .8437624610086617

; We can compare it with the analytical solution:
(sin 1.)
; => .8414709848078965



; THe book also introduces a signal processing model for solving
; differential equations in the form:

; dy/dx = f(y)

; The procedure is defined as:

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

; If we define y to be f(x) = x, we get:

; dy/dx = y

; We now that the solution for that differential equation is:

; y = e^x

; Let's try to evaluate our `solve` procedure to find it:

(define solution-stream (solve (lambda (x) x) 1 .001))

(stream-ref solution-stream 1000)
; => 2.7142097225133828


