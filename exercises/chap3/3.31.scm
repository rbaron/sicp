; Analyzing the initialization process on `accept-action-procedure!` on
; `make-wire`.

; Here is the `make-wire` procedure:

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

; In special, we're interested in the `accept-action-procedure!`, which
; adds the `proc` argument to `action-procedures` _and_ immediately
; calls it too.

; The exercise asks us to investigate the effect of removing the call
; `(proc)` from `accept-action-procedure!` and analyze the effect of it
; by evaluating the half-adder example.

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

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

(define (logical-and s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        (else 0)))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or s1 s2)
  (cond ((and (= s1 0) (= s2 0)) 0)
        (else 1)))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))


; Let's figure out what's happening by tracing the call to `half-adder`.

(define A (make-wire))
(define B (make-wire))
(define S (make-wire))
(define C (make-wire))


; Evaluating
(half-adder A B S C)

; Would internally define:
; (define D (make-wire))
; (define E (make-wire))

; would have the following effects:

; (or-gate A B D)
; => this would add the `or-action-procedure` to both A and B wires.
; Both procedures would also be immediately called, calling `after-delay`,
; which would add the action of setting D to 0 after `or-gate-delay` inside
; `the-agenda`.

; (and-gate A B C)
; => adds the `and-action-procedure` to both A and B. Immediatelly
; calls them, adding procedures to set C to 0 after `and-gate-delay`
; inside `the-agenda`.

; (inverter C E)
; => adds the `invert-input` procedure to C. Immediately calls it,
; adding the procedure for setting E to 1 after `inverter-delay`
; inside `the-agenda`.

; (and-gate D E S)
; => adds the `and-action-procedure` to both D and E. Immediatelly
; calls them, adding the procedure to set S to 0 after `and-gate-delay`
; inside `the-agenda`. The procedure inserted on E wire will also
; be called after `inverter-delay`, since the value of
; E will be changed to 1. This will cause a new procedure to be inserted
; inside `the-agenda`, which keeps ouput S at 0 after `and-gate-delay`.

; We can now simulate the initial system with

; (propagate)

; So, at this point, with both inputs A and B having their default value
; of 0, the state of the wires will be:

; A: 0
; B: 0
; C: 0
; D: 0
; E: 1
; S: 0

; Let's try changing the input A to 1 with

; (set-signal! A 1)

; and start the simulation
; (propagate)

; `(set-signal! A 1)`, which will cause `or-action-procedure` to be
; called, adding a procedure for setting D to 1 after `or-gate-delay`
; _and_ a procedure for setting C to 0 after `and-gate-delay` inside
; `the-agenda`.

; The change on D will trigger and `and-action-procedure` to be evaluated, which will
; add a procedure to set the output S to 1 (since E is 1) after `and-gate-delay` inside
; `the-agenda`.

; So now we'd have:

; A: 1
; B: 0
; C: 0
; D: 0
; E: 1
; S: 1

; This makes it clear why the immediate call `(proc)` is necessary inside
; `accept-action-procedure!`. If we hadn't evaluated `proc` upon initialization,
; we would be left in an "inconsistent" state, which would cause setting A to
; 1 to yield a false result (S would be 0).
