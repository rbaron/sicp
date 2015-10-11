; Resettable random number generator.

; Let's modify the `rand` procedure so we can reset it internal
; state to a given value.


; Dummy random stubs
(define random-init .123)
(define (rand-update x) (+ x .321))

; Modified `rand`
(define rand
  (let ((x random-init))
    (lambda (task)
      (cond ((eq? task 'generate)
              (begin (set! x (rand-update x))
                    x))
            ((eq? task 'reset)
              (lambda (new-val) (set! x new-val)))))))


; Testing

(rand 'generate)
; => .444

(rand 'generate)
; => .765

(rand 'generate)
; => 1.086

((rand 'reset) random-init)

(rand 'generate)
; => .444

(rand 'generate)
; => .765

(rand 'generate)
; => 1.086
