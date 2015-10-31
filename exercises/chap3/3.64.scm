; There is always a `tolerance` to how much you can `take`!

; Let's define a function that receives a stream and
; returns the element such that the difference between
; that element and the previous one is less than a given
; tolerance
(define (stream-limit s1 tolerance)
    (let ((h1 (stream-car s1))
          (h2 (stream-car (stream-cdr s1))))
      (if (< (abs (- h1 h2)) tolerance)
        h2
        (stream-limit (stream-cdr s1) tolerance))))

; Testing

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (average a1 a2)
  (/ (+ a1 a2) 2.0))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (display-inf-stream s n-terms)
  (define (iter s n-terms)
    (if (> n-terms 0)
      (begin (display " ")(display (stream-car s))
             (iter (stream-cdr s) (- n-terms 1)))))
  (newline)
  (iter s n-terms))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(sqrt 2 .01)
; => 1.4142156862745097

(sqrt 2 .000000001)
; => 1.414213562373095

