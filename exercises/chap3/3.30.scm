; Implementing a ripple-carry adder

; Given the procedures:

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

; We can implement a k-bit ripple-carry adder in terms of
; those procedures:

(define (ripple-carry-adder as bs ss c)
  (if (null? as)
    (set-signal! c 0)
    (let ((ck (make-wire)))
      (full-adder (car as) (car bs) ck (car ss) c)
      (ripple-carry (cdr as) (cdr bs) (cdr ss) ck))))
