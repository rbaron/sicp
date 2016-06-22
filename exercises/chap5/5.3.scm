; Data-path and controller definition in register-machine language
; for sqrt using Newton's method

; a. Assuming improve and good-enough? are given

(controller
    (assign x (op read))
    (assign guess (const 1.0))
  test-good
    (test (op good-enough?) (reg  guess) (reg x))
    (branch (label done))
    (assign guess (op improve) (reg guess) (reg x))
    (goto (label test-good))
  done
    (perform (op print) (reg guess)))


; b. Inline improve and goodenough?

(controller
    (assign x (op read))
    (assign guess (const 1.0))

  good-enough?
    (assign sq (op square) (reg guess))
    (assign m (op -) (reg square) (reg x))
    (assign a (op abs) (reg m))
    (test (op <) (reg a) (const 0.001))
    (branch (label done))

  improve
    (assign div (op /) (reg x) (reg guess))
    (assign guess (op average) (reg guess) (reg div))
    (goto (label good-enough?))

  done
    (perform (op print) (reg guess)))

; Note that we could do the same with considerably fewer registers.
; We could, for instance, use a single temporary register for all
; intermediate results in good-enough? and improve.
