; Making joint accounts with different passwords


; Modified `make-account` from exercise 3.3
(define (make-account balance password)

  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))

  (define (deposit amount)
      (begin (set! balance (+ balance amount))
             balance))

  ; Modification: a new dispatch procedure that operates
  ; with a new password
  (define (make-joint new-pass)
    (make-dispatcher new-pass))

  (define (make-dispatcher password)
    (lambda (user-supplied-password task)
      (if (eq? user-supplied-password password)
        (cond ((eq? task 'withdraw) withdraw)
              ((eq? task 'deposit) deposit)
              ((eq? task 'make-joint) make-joint)
              (else "Unsupported opperation"))
        (lambda (args . rest) "Incorrect password"))))

  ; The "original" bank account operates on the default password
  (make-dispatcher password))


; Solution

; We will want the new bank account to the exact "same" account
; as the original one, but, at the same time, support a different
; password.

;(define (make-joint account password new-password)
(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)
; => 60

; Let's create a joint account
(define acc-joint ((acc 'secret-password 'make-joint) 'new-pass))

; The account now hold a balance of 60. Let's withdraw 20 from the
; joint account. We expect the result to be 40.
((acc-joint 'new-pass 'withdraw) 20)
; => 40

; Trying to withdraw from the joint with the "original" password
((acc-joint 'secret-password 'withdraw) 20)
; => "Incorrect password"

; The account now has a balance of 40. Let's withdraw 50 from the original
; account. We should expect a message telling us that there aren't enough
; funds.
((acc 'secret-password 'withdraw) 50)
; => "Insufficient funds"

