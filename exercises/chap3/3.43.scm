; Analyzing the `exchange` and `serialized-exchange` procedure

; Suppose we have 3 bank accounts:
; acc1 contains $10
; acc2 contains $20
; acc3 contains $30

; Let's start with the first implementation of `exchange`:
(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))


; If everything was done serially, `exchange` would only ever
; swap the balance of two accounts. In other words, the values
; 10, 20 and 30 would just be swapped between accounts. So the
; only three possible values any account will ever hold are
; 10, 20 and 30.


; Let's now move to a parallel execution of two concurrent
; `exchange` procedures:

(exchange acc2 acc1)
(exchange acc3 acc2)

; Each `exchange` call will execute the following tasks:
; get balance for 1st account
; get balance for 2nd account and calculate the difference
; withdraw on the 1st account
; deposit on the 1st account

; One possible scenario is:

; t         exchange1               exchange2
;
; 0         get b2=20
; 1                                   get b3=30
; 2         get b1=10 (diff=10)
; 3                                   get b2=20 (diff=10)
; 4         withdraw b2 (b2=10)
; 5                                   withdraw b3 (b3=20)
; 6         deposit b1 (b1=20)
; 7                                   deposit b2 (b2=20)

; => In the end, we'd get: b1=20, b2=20, b3=20 (the sum is preserved)

; Another possible scenario is:

; t         exchange1               exchange2
;
; 0         get b2=20
; 1                                   get b3=30
; 2                                   get b2=20 (diff=10)
; 3                                   withdraw b3 (b3=20)
; 4                                   deposit b2 (b2=30)
; 5         get b1=10 (diff=10)
; 6         withdraw b2 (b2=20)
; 7         deposit b1 (b1=20)

; => In the end, we'd get: b1=20, b2=20, b3=20 (the sum is preserved)

; If we don't serialize the transactions on individual accounts, this
; conservative property fails to hold. Let's try to create a pair of
; non-conservative concurrent calls.

; Let's thing abount two concurrent calls to `(withdraw b2 10)`.

; Withdraw has to do the following tasks:
; Access b2's `balance`
; Set b2's `balance` to the accessed `balance` + `amount`


; t         withdraw1                withdraw2
;
; 0         get balance=20
; 1                                get balance=20
; 2         set balance=30
; 3                                set balance=30

; In the beggining we had 20 + 10 + 10 (current balance + 2*10 from deposits).
; In the end we have only 30! This is a "non-conservative" concurrent example.
; So, using this rationale, we can argue that by not serializing the individual
; transactions, we made our system non-conservative.
