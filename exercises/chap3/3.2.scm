; Using internal state for testing - keeping track of how many
; times a procedure is called

(define (make-monitored f)
  (define call-count 0)
  (lambda (input)
    (cond ((eq? input 'how-many-calls?) call-count)
          ((eq? input 'reset-count) (set! call-count 0))
          (else (begin (set! call-count (+ call-count 1))
                       (f input))))))


; Let's monitor the `sqrt` procedure
(define s (make-monitored sqrt))

(s 100)
; => 10

(s 'how-many-calls?)
; => 1

(s 'how-many-calls?)
; => 1

(s 169)
; => 13

(s 'how-many-calls?)
; => 2

; Resetting the counter
(s 'reset-count)

(s 'how-many-calls?)
; => 0
