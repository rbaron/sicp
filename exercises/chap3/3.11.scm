; Using the enviroment model to analyze procedures with both local state
; and internal definitions.

; Let's take a look at the message passing implementation for accounts from
; section 3.1.1:

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

; First of all, the above `define` will be evaluated in the global
; frame, which will create a enviroment structure such as:

;                +--------------------------------+
;                |                                |
; Global env ->  |  make-account: +--+            |
;                |                   |            |
;                |                   |            |
;                +---------------------------+----+
;                                    |       ^
;                                    |       |
;                                 +--v--+    |
;                       +----------o | o-----+
;                       |         +-----+
;                       v
;                   parameters: balance
;                   body: ...
;

; Let's now analyze what will happen with the following call:

(define acc (make-account 50))

; The subexpression `(make-account 50)` will be evaluated first. A new
; enviroment E1 will be created where the parameters of `make-account`
; will be bound and body of `make-account` will be evaluated. This will
; cause the internal `define` expressions to be evaluated inside E1, thus
; defining the internal procedures. The `dispatch` internal procedure is
; then returned. It is a procedure object, whose enclosing enviroment is
; E1

;                +--------------------------------+
;                |                                |
;                |  make-account: ...             |
; Global env ->  |                                |
;                |  acc: +-+                      |
;                |         |                      |
;                +--------------------------+-----+
;                          |                ^
;                          |                |
;                          |                |
;                          |         +------+--------+
;                          v         |               |
;                       +--+--+      | balance: 50   |
;              +---------o | o------->               |
;              |        +-----+      | withdraw: ... |
;              v                     |               | -> E1
;      parameters: m                 | deposit: ...  |
;      body: (dispatch body)         |               |
;                                    | dipatch:      |
;                                    |               |
;                                    |               |
;                                    +---------------+

; Let's now analyze the call to:

((acc 'deposit) 40)

; This will cause `acc` to be looked up in the global frame. `acc`, in its
; turn, is in fact a procedure defined in E1, `dispatch`. Upon execution,
; a new enviroment, E2, will be created and the formal parameters will be
; bound. It's important to notice that E2 will have as enclosing enviroment
; the one in which `dispatch` was defined. We'll have the following structure:

;                +--------------------------------+
;                |                                |
;                |  make-account: ...             |
; Global env ->  |                                |
;                |  acc: +-+                      |
;                |         |                      |
;                +--------------------------+-----+
;                          |                ^
;                          |                |
;                          |                |
;                          |         +------+--------+
;                          ^         |               |
;                       +-----+      | balance: 50   |
;              +--------+o | o+------>               |
;              |        +--+--+      | withdraw: ... |
;              v                     |               | -> E1
;      parameters: m                 | deposit: ...  |
;      body: (dispatch body)         |               |
;                                    | dipatch:      |
;                                    |               |
;                                    |               |
;                                    +-----^---------+
;                                          |
;                                          |
;                                    +-----+---------+
;                                    |               |
;                                    | m: 'deposit   | -> E2
;                                    |               |
;                                    +---------------+

; This will cause `dispatch` to return the `deposit` procedure in E1,
; and it will create yet a new enviroment, E3, in which it will be
; evaluated:

;                +--------------------------------+
;                |                                |
;                |  make-account: ...             |
; Global env ->  |                                |
;                |  acc: +-+                      |
;                |         |                      |
;                +--------------------------+-----+
;                          |                ^
;                          |                |
;                          |                |
;                          |         +------+--------+
;                          ^         |               |
;                       +-----+      | balance: 50   |
;              +--------+o | o+------>               |
;              |        +--+--+      | withdraw: ... |
;              v                     |               | -> E1
;      parameters: m                 | deposit: ...  |
;      body: (dispatch body)         |               |
;                                    | dipatch:      |
;                                    |               |
;                                    |               |
;                                    +-----^---------+
;                                          |
;                                          |
;                                    +-----+---------+
;                                    |               |
;                                    | amount: 40    | -> E3
;                                    |               |
;                                    +---------------+

; This will cause the value `balance` in E1 to be incremented by 40.

; The call:
((acc 'withdraw) 40)

; Works in an analogous way.

; Where is the local state for acc kept?
; => It's kept in the frame E1.

; Suppose we define another account

(define acc2 (make-account 100))

; How are the local states for the two accounts kept distinct?
; => The state for `acc2` will be kept in a separate frame.

; Which parts of the environment structure are shared between `acc`
; and `acc2`?
; => The code part of the procedure objects could point to the same text.
