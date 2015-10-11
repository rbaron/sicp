; Monte Carlo integral estimation. Let's estimate `pi` using the MC method.

; We're going to estimate `pi` by using the relative area of a unit
; circle and it's bounding rectangle.


; For a given `x`, `y`, returns true if they fall into the circle,
; false otherwise. This represents a circle with radius .5 and
; center at (.5, .5)
(define (unit-circle-predicate x y)
  (< (+ (expt (- x .5) 2) (expt (- y .5) 2)) (expt .5 2)))

(define (unit-circle-test)
  (unit-circle-predicate (random-in-range 0. 1.)
                         (random-in-range 0. 1.)))

(define (estimate-circle-area trials)
  ; Multiply the frequency of the times `unit-circle-predicate`
  ; evaluates to true times the area of its unit bounding square
  (* 1.0 (monte-carlo trials unit-circle-test)))

(define (estimate-pi trials)
  (/ (estimate-circle-area trials) (expt .5 2)))

; Given procedures

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

; END OF of given procedures

; Testing

(estimate-pi 100)
; => 3.2

(estimate-pi 10000)
; => 3.1576

(estimate-pi 100000)
; => 3.14484

(estimate-pi 1000000)
; => 3.140452
