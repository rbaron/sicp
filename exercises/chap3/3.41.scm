; Protecting read operations

; Ben Bitdiddle argues that a better implementation of
; our bank account wouldbe:

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance)

             ;; Ben added this `protected` wrapper
             ((protected (lambda () balance))))
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

; What kind of anomalous behavior would this protect us against?

; => Concurrent read operations are not a concern. Concurrent read
; and write operations _could_ produce weird values for balance _if_
; the write operations from `withdraw` or `deposit` aren't atomic.
; If writing a new value to `balance` takes more than one step,
; interleaving a read operation in between those steps could cause
; anomalous values of `balance` to be read.

; So, in short, as is often the case, it depends. Whether write operations
; on `balance` would be atomic or not impacts the way we should design
; our system. Concurrency is hard!
