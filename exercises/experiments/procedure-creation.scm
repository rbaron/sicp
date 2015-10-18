; Let's investigate further how procedures are created.

; Quoting the book:

; Procedures are created in one way only: by evaluating a lambda
; expression. This produces a procedure whose code is obtained from
; the text of the lambda expression and whose environment is the
; environment in which the lambda expression was evaluated to
; produce the procedure.

(define (f1 a)
  (+ a 1))

(f1 1)
; => 2

; The above definition is just syntatic sugar for:

(define f2
  (lambda (a)
    (+ a 1)))

(f2 1)
; => 2


; Let's see what happens when we actually cause a lambda
; to be evaluated _during_ the definition of a procedure:

(define f3
  ((lambda ()
    (lambda (a)
    (+ a 1)))))

(f3 1)
; => 2

; The outer lambda was evaluated during the defition of f3, and
; the result of that evaluation is actually the created procedure.


; This allows us to (dangerously) mantain local state inside procedures,
; such as:

(define f4
  ((lambda (state)
    (lambda (a)
      (begin (set! state (+ state 1))
             (+ a state)))) 0))

; Upon definition, the otter lambda will be executed, creating a new
; enviroment E1 (which has the global enviroment as its enclosing
; enviroment) in which `state` will be bound to 0. The returned procedure,
; the inner lambda will be defined in the global frame, but will have
; as its enviroment E1.

(f4 1)
; => 2
; This call modified `state` inside E1

(f4 1)
; => 3
; This call modified `state` inside E1

; This means we lost what the author calls "referential transparency", since
; both calls have side effects. The order in which they're called matters.


; A more interesting pattern is perhaps levegaring the idea of procedures
; keeping internal states to create scenarios in which internal state is
; not _shared_ between objects but are created in a way to be independent
; of each other. A good example from the book is creating bank accounts:

(define make-deposit-bank-account
  (lambda (initial-balance)
    ; Deposit
    (lambda (amount)
      (begin (set! initial-balance (+ initial-balance amount))
             initial-balance))))

; In this case, the definition of `make-deposit-bank-account` does not create
; a persistent enviroment. Now, enviroments will be created upon _executing_
; `make-deposit-bank-account`. The important thing is that _each_ call to
; this procedure will create an isolated enviroment.

(define b1 (make-deposit-bank-account 100))
; => This created a new enviroment E1 in which balance is bound to 100.

(define b2 (make-deposit-bank-account 100))
; => This created a new enviroment E2 in which balance is bound to 100. E2
; is completely isolated from E1, as we can see:

(b1 50)
; => 150

(b1 20)
; => 170

(b2 10)
; => 110
