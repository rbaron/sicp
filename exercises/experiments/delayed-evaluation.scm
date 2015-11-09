; Let's investigate further how delayed evaluation works

; Let's create a procedure that returns a promise to add
; a given`param` with an inner `inner-param`
(define (make-promise param)
  (define inner-param 10)
  (delay (+ inner-param param)))

(define p (make-promise 20))

p
; => #[promise 13]

(force p)
; => 30

; This means the promise object has knowledge about the
; environment in which it was defined! What I think it's
; happening is analogous to the procedure call: a new frame
; is created for the promise, having as enclosing environment
; the one in which the promise was created.

; Let's investigate another case:

(define (make-promise2)

  (define inner 10)

  (define promise (delay (+ inner 20)))

  (define (set value) (set! inner value))

  (define (dispatch msg)
    (cond ((eq? msg 'set) set)
          ((eq? msg 'get) promise)))
  dispatch)

(define p2 (make-promise2))

p2
; => #[compound-procedure 14 dispatch]

(p2 'get)
; => #[promise 15]

((p2 'set) 20)

(force (p2 'get))
; => 40

; This shows how `inner` was lazily evaluated
; on `promise`.

; As the book says, `delay` could've been implemented by

(define (delay2 expr) (lambda () expr))
(define (force2 delayed-expr) (delayed-expr))

; But we would lose the lazy evaluation, since `expr`
; would be evaluated when the arguments for `delay2`
; were evaluated. All other things should be the same.

; Let's make sure it works the same way:

(define (make-promise3)

  (define inner 10)

  (define promise (delay2 (+ inner 20)))

  (define (set value) (set! inner value))

  (define (dispatch msg)
    (cond ((eq? msg 'set) set)
          ((eq? msg 'get) promise)))
  dispatch)

(define p3 (make-promise3))

p3
; => #[compound-procedure 16 dispatch]

(p3 'get)
; => #[compound-procedure 17]

((p3 'set) 20)

(force2 (p3 'get))
; => 30

; This is ddiferent from 40 using `delay` and `force`!
; The reason is that the argument was evaluated upon calling
; the procedure `delay2`. In this case, p3 is simply:

; (lambda () 30)

; Instead of being:

; (lambda () (+ inner 20))

; In the previous, properly delayed case.
