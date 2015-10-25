; Deadlock avoidance technique

; Let's try to avoid a deadlock scenario in our `serialized-exchange`
; procedure. The idea is to implement a "priority" to figure out
; which process should acquire the lock in case both are locked
; waiting for the other one.

; Let's rewrite `serialized-exchange` to incorporate that idea:
(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer))
        (acc1-priority (get-priority account1))
        (acc2-priority (get-priority account2)))
    (if (< acc2-priority acc1-priority)
      ((serializer1 (serializer2 exchange))
         account1
         account2)
      ((serializer2 (serializer1 exchange))
         account1
         account2))))

; This will ensure that the following two calls, for instance:

; (serialized-exchange acc1 acc2)
; (serialized-exchange acc2 acc1)

; Will both try, in order:

; - Serialize access to acc1
; - Serialize access to acc2

; So there is no possibilty for one process waiting for the
; resources from the other one. Deadlock avoided!

; For this to work, we'd have to implement and expose
; the unique priority of each account. We can use some
; sort of numerical id to represent the priority.

(define (make-account balance id)

  ; Same withdraw, deposit procedures

  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          ((eq? m 'priority) id)))

  dispatch)
