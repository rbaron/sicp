; Using a persistent serialized procedures inside the bank account

; Ben argues that it's a waste to create new serializers every time
; `dispatch` is called. He proposes:

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
    (let ((protected-withdraw (protected withdraw))
          (protected-deposit (protected deposit)))
      (define (dispatch m)
        (cond ((eq? m 'withdraw) protected-withdraw)
              ((eq? m 'deposit) protected-deposit)
              ((eq? m 'balance) balance)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m))))
      dispatch)))

; That is, using persistent serialized versions or `withdraw`
; and `deposit`. Would this bahave differently from the previous
; serialization method?

; => No. The serializer `protected` has an internal state that
; is responsible for synchronizing/serializing the calls to
; procedures associated with it. In both cases, we are creating
; serialized procedures in the same way. No change should be
; detected.
