; Redoing exercise 3.5 in terms of streams

; The idea is to define a "test" that will receive a random pair
; of coordinates and test whether that pair falls inside the unit
; circle. The ratio of the times it falls into to all the times
; should be proportional to the area of the circle. Using that
; fact, we can estimate pi.

; Given procedures

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

; Let's produce a stream of random numbers between 0. and 1.
(define rand-stream
  (cons-stream (random 1.)
               (stream-map (lambda (e) (random 1.)) rand-stream)))

(define (rand-pairs stream)
  (cons-stream (cons (stream-car stream) (stream-car (stream-cdr stream)))
               (rand-pairs (stream-cdr (stream-cdr stream)))))

; And now a we generate a stream of "tests" for whether random points
; fall inside or outside the circle
(define rand-pairs-in-circle? (stream-map
                                (lambda (pair) (unit-circle-predicate (car pair) (cdr pair)))
                                (rand-pairs rand-stream)))

; And finally we can use `monte-carlo` to estimate pi
(define estimate-pi
  (stream-map (lambda (fraction) (/ fraction  (expt .5 2)))
              (monte-carlo rand-pairs-in-circle? 0 0)))

(stream-ref estimate-pi 100)
; => 3.00990099009901

(stream-ref estimate-pi 100000)
; => 3.148568514314857
