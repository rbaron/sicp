; Why is a FIFO better than a LIFO inside a time segment?

; Let's trace the behavior of the `and-gate`.

(define A (make-wire))
(define B (make-wire))
(define O (make-wire))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(and-gate A B O)

; Whis would insert two procedures in `the-agenda` at
; time `(+ 0 and-gate-delay)`:

; 1. (set-signal! O 0)
; 2. (set-signal! O 0)

; Let's now consider the case in which:

; A: 0
; B: 1

; And we set, at the same time point `t`:

; A: 1
; B: 0

; Setting A to 1 will cause the `and-action-procedure` added
; to the wire A to be executed. It cause `after-delay` to be
; called with:

; (after-delay and-gate-delay
;    (lambda () (set-signal! O (logical-and 1 1))))

; (after-delay and-gate-delay
;    (lambda () (set-signal! O 1)))

; since B is still at 1.

; Setting B to 0 will cause the `and-action-procedure` added
; to the wire B to be executed. It cause `after-delay` to be
; called with:

; (after-delay and-gate-delay
;    (lambda () (set-signal! O (logical-and 1 0))))

; (after-delay and-gate-delay
;    (lambda () (set-signal! O 0)))


; Now, at the time `t + and-gate-delay`, O will be quickly
; set to 1 and immediately set to 0, since the execution order
; is a FIFO.


; Now, if the execution order was a LIFO (stack, for instance),
; we would have:

; Let's now consider the case in which:

; A: 0
; B: 1

; And we set, at the same time point `t`:

; A: 1
; B: 0

; Now, instead of as above, we'd evaluate the lambdas inserted
; with `after-delay`, in the other order.

; First, the lambda inserted by setting B to 0:
; (after-delay and-gate-delay
;    (lambda () (set-signal! O (logical-and 1 0))))

; (after-delay and-gate-delay
;    (lambda () (set-signal! O 0)))

; First, the lambda inserted by setting A to 1:
; (after-delay and-gate-delay
;    (lambda () (set-signal! O (logical-and 1 1))))

; (after-delay and-gate-delay
;    (lambda () (set-signal! O 1)))

; This would cause O to be 1 at the end of the process, which is wrong!
