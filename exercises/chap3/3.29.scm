; Alternative `or-gate` implementation

; We can define the OR logical operation in terms of
; NOT and AND. A direct application of De Morgan's
; theorem yields:

; A or B == ¬( ¬A and ¬B )

; Note that we can actually build `or-gate` in terms of
; more "primitive" procedures, just like `half-adder` was
; defined
(define (or-gate a1 a2 output)
  (let ((w1 (make-wire))
        (w2 (make-wire))
        (w3 (make-wire)))
    (inverter a1 w1)
    (inverter a2 w2)
    (or-gate w1 w2 w3)
    (inverter w3 output)))


; The total delay is the sum of the last `inverter-delay`, the
; `and-gate-delay` and one of the first two `inverter-delay`, since
; the two may happen at the same time.
