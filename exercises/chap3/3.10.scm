; Analyzing the alternative version of `make-withdraw` using the enviroment
; model.

; Here's the alternative version, using `let` for local bindings:
(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

; As the exercise suggests, we can interpret the `let` expression as syntatic
; sugar for:
(define (make-withdraw initial-amount)
  ((lambda (balance)
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds")))
    (initial-amount)))

; When the `define` above is evaluated, it creates a binding in the global
; frame whose name is `make-withdraw` and the value is a procedure object
; as follows:

;                     +------------------------------+
;                     |                              |
;Global Enviroment +> |  make-withdraw:  +---+       |
;                     |                      |       |
;                     |                      |       |
;                     |                      |       |
;                     +----------------------------+-+
;                                            |     ^
;                                            |     |
;                                            v     |
;                                         +-----+  |
;                                     +---|o | o|--+
;                                     |   +--+--+
;                                     v
;                                  parameters: initial-amount
;                                  body:  (let ((balance initial-amount))
;                                      (lambda (amount)
;                                        (if (>= balance amount)
;                                            (begin (set! balance (- balance amount))
;                                                   balance)
;                                            "Insufficient funds"))))
;

; Let's see what's going on when we call

(define W1 (make-withdraw 100))

; To evaluate `make-withdraw`, a new eviroment E1 is created with the formal
; parameter `initial-amount` bound to 100. In the de-sugared version of `make-withdraw`,
; we can see a `lambda` expression will be evaulated right away. It will, in its turn,
; create yet another frame E2 and bind the formal parameter `balance` to `initial-amount`,
; which is define in E1 as 100.a

;                                     +------------------------------+
;                                     |                              |
;                Global Enviroment +> |  make-withdraw: ...          |
;                                     |                              |
;                                     |  W1: +-+                     |
;                                     |        |                     |
;                                     +--------------------------^---+
;                                              |                 |
;                                              |                 |
;                                              |            +----+----------------+
;                                   +----------+            |                     |
;                                   |                       | initial-amount: 100 | -> E1
;                                   v                       |                     |
;                                +-----+                    +-----------^---------+
;                                |o | o+----------+                     |
;                                +--+--+          |         +-----------+---------+
;                                 |               |         |                     |
;                                 v               |         | balance: 100        | -> E2
; parameters: amount                              |         |                     |
; body: (if (>= balance amount)                   |         +-----------^---------+
; (begin (set! balance (- balance amount))        |                     |
;   balance)                                      +---------------------+
; "Insufficient funds"))

; Let's analyze the call:

(W1 50)

; This will create a new enviroment, E3, whose enclosing enviroment is the one
; that `W1` points to - E2. The parameter `amount` will be bound to 50 in that
; enviroment:

;                                     +------------------------------+
;                                     |                              |
;                Global Enviroment +> |  make-withdraw: ...          |
;                                     |                              |
;                                     |  W1: +-+                     |
;                                     |        |                     |
;                                     +--------------------------^---+
;                                              |                 |
;                                              |                 |
;                                              |            +----+----------------+
;                                   +----------+            |                     |
;                                   |                       | initial-amount: 100 | -> E1
;                                   v                       |                     |
;                                +-----+                    +-----------^---------+
;                                |o | o+----------+                     |
;                                ++-+--+          |         +-----------+---------+
;                                 |               |         |                     |
;                                 v               |         | balance: 100        | -> E2
; parameters:                                     |         |                     |
; body: (if (>= balance amount)                   |         +-----------^-----^---+
; (begin (set! balance (- balance amount))        |                     |     |
;   balance)                                      +---------------------+     |
; "Insufficient funds"))                                                      |
;                                                            +----------------+----+
;                                                            |                     |
;                                                            | amount: 50          | -> E3
;                                                            |                     |
;                                                            +---------------------+

; Then the body of `W1` will be evaluated inside E3. This will call `set! balance 50`, which
; will modify the `balance` binding on E2. After the execution, E3 is no longer relevant,
; since there are no other frames pointing to it.

; The evaluation of:

(define W2 (make-withdraw 100))

; is completely analogous. New frames will be created, completely apart from the ones
; created in the process of evaluating `W1`. The only common part of `W1` and `W2` will
; be the code, since they both point to the same text (the first element of the procedure
; objects).
