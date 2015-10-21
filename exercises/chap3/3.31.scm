; Analyzing the initialization process on `accept-action-procedure!` on
; `make-wire`.

; In special, we're interested in the `accept-action-procedure!`, which
; adds the `proc` argument to `action-procedures` _and_ immediately
; calls it too.

; The exercise asks us to investigate the effect of removing the call
; `(proc)` from `accept-action-procedure!` and analyze the effect of it
; by evaluating the half-adder example.


; Let's figure out what's happening by tracing the call to `half-adder`.

; (define A (make-wire))
; (define B (make-wire))
; (define S (make-wire))
; (define C (make-wire))

; Evaluating
; (half-adder A B S C)

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


; Let's simulate it:

; ==========================================================
;                    START OF CODE THROW UP
; ==========================================================
(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))
(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))
(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))
(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

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

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))
(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))
(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "  New-value = ")
                 (display (get-signal wire)))))

; ==========================================================
;                    END OF CODE THROW UP
; ==========================================================


; This is the relevant part for our simulation. Pay attention
; specially to the `(proc)` line inside `make-wire`. Commenting
; it out will yield the results we predicted above!

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (probe 'D d)
    (probe 'E e)
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))

      ; Comment one of the following two lines to get different results!
      (proc))
      ;)

    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))


(define inverter-delay 1)
(define or-gate-delay 2)
(define and-gate-delay 3)
(define the-agenda (make-agenda))

(define A (make-wire))
(define B (make-wire))
(define S (make-wire))
(define C (make-wire))

(probe 'A A)
(probe 'B B)
(probe 'S S)
(probe 'C C)

(half-adder A B S C)

(set-signal! A 1)

(propagate)

; Depending on whether we comment out `(proc)` inside `make-wire`
; or not, S will either be 1 (correct) or 0 (incorrec)
(get-signal S)
