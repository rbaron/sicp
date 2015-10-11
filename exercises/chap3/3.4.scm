; Calling the cops if a protected operation is called
; seven consecutive times with the wrong password

(define (make-account balance password)

  (define wrong-consec-count 0)
  (define max-wrong-consec-count 7)

  (define (withdraw amount)
    (if (> balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))

  (define (deposit amount)
      (begin (set! balance (+ balance amount))
             balance))

  (define (call-the-cops)
    (lambda (args . rest) "TOO MANY WRONG ATTEMPTS! CALLING THE COPS!"))

  (define (dispatch user-supplied-password task)
    (if (eq? user-supplied-password password)
        (begin (set! wrong-consec-count 0)
               (cond ((eq? task 'withdraw) withdraw)
                     ((eq? task 'deposit) deposit)
                     (else "Unsupported opperation")))
      (begin (set! wrong-consec-count (+ wrong-consec-count 1))
             (if (> wrong-consec-count max-wrong-consec-count)
               (call-the-cops)
               (lambda (args . rest) "Incorrect password")))))

  dispatch)


; Testing
(define acc (make-account 100 'secret-password))

(define (repeat-call n-times proc)
  (newline)
  (display "Call result: ")
  (display (proc))
  (if (= n-times 1)
    "Done repeating procedure"
    (repeat-call (- n-times 1) proc)))

; Trying to withdraw 7 times
(repeat-call 7
             (lambda ()
               ((acc 'wrong-pass 'withdraw) 10)))
; => Call result: Incorrect password
; => Call result: Incorrect password
; => Call result: Incorrect password
; => Call result: Incorrect password
; => Call result: Incorrect password
; => Call result: Incorrect password
; => Call result: Incorrect password
; => "Done repeating procedure"


; 8th consecute withdraw attempt with the wrong pass:
((acc 'wrong-pass 'withdraw) 10)
; => "TOO MANY WRONG ATTEMPTS! CALLING THE COPS!"

