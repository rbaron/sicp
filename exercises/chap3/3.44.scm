; Concurrently transferring money between two accounts

; Let's analyze Ben's `transfer` implementation:
(define (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))

; Since the procedures for withdrawing and depositing money are
; serialized for each individual account, we do not neet to worry
; abount concurrency here.

; The main different between `transfer` and `exchange` resides in
; the fact that we are now transferring a _fixed_ amount of money
; between accounts. In the exchange scenario, we had to read the
; initial balance for each account. Between reading and writing
; the new values, the state could have been changed from another
; process. In this case, it doesn't matter, since we are, as said,
; transferring a fixed amount of money. So Louis is wrong.
