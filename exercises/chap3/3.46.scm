; Analyzing a non-atomic mutex implementation

; Given mutex implementation

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

; If `test-and-set!` cannot be executed atomically, we could
; observe two processes acquiring the lock at the same time.
; Here's one possible scenario:

; Initially the lock is free. That means `cell` inside
; `test-and-set!` will be `(list false)`. Let's say we
; have two processes trying to acquire the lock.


; t         process1                          process2
;
; 0         read cell = false
; 1                                       read cell = false
; 2         set cell = true, ret false
; 3                                       set cell = true, ret false

; This would cause both process1 and process2 to "acquire" the lock.
