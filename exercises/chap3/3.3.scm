; Password-protected `make-account`

(define (make-account balance password)

  (define (withdraw amount)
    (if (> balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))

  (define (deposit amount)
      (begin (set! balance (+ balance amount))
             balance))

  (define (dispatch user-supplied-password task)
    (if (eq? user-supplied-password password)
      (cond ((eq? task 'withdraw) withdraw)
            ((eq? task 'deposit) deposit)
            (else "Unsupported opperation"))

      ; I'm returning a `lambda` instead of a simple pass
      ; So the user can still use calls such as:
      ; `((acc 'wrong-password 'withdraw) 40)`
      ; And fail gracefully
      (lambda (args . rest) "Incorrect password")))

  dispatch)

; Testing

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)
; => 60

((acc 'secret-password 'deposit) 15)
; => 75

((acc 'some-other-password 'deposit) 50)
; => "Incorrect password"
