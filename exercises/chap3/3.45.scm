; Apllying _and_ exporting accounts serializers

; Loius proposes that we serialize the withdraw and deposit
; procedures in addition to exposing the serializer. His
; implementation of this logic is:

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (balance-serializer withdraw))
            ((eq? m 'deposit) (balance-serializer deposit))
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

; What's the problem with it, specially whencalling `serialized-exchange`?

; Let's take a look at `serialized-exchange`:

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))

; Let's analyze the following program:
; (define acc1 (make-account-and-serializer 100))
; (define acc2 (make-account-and-serializer 200))
;
; (serialized-exchange acc1 acc2)

; This will cause `exchange` to be serialized with both
; `acc1`'s and `acc2`'s serializers. The problem is that
; this new double-serialized `exchange` procedure has inner
; calls to `withdraw` and `deposit`, both of which are also
; serialized. This will cause a the program to hang, since
; `exchange` would be waiting to access `withdraw`, which will
; be waiting for `exchange`. This a case of dead lock.
