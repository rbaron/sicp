; Saving stack operations

; Usual evaluation:

; Operator evaluation: env
; Operand sequence evaluation: proc
; Each operant evaluation: env, argl

; a. (f 'x 'y)

; Operator evaluation: f is already a symbol that points to the procedure. A simple lookup-variable-value
;                      gets the job done. No need to save the env.
; Operand sequence evaluation: Operands are self-evaluating quoted expressions. They evaluate to themselves
;                              so no stack operation is needed.
; Each operand evaluation: No need to save any register for the same reason.

; b. ((f) 'x 'y)

; Operator evaluation: Operator evaluation is nested, so it may change env. On the other hand, no operand
;                      depends on env, so we don't need to save it.
; Operand sequence evaluation: None
; Each operand evaluation: None

; c. (f (g 'x) y)

; Operator evaluation: None
; Operand sequence evaluation: We need to save proc since there is a nested application that will change it.
; Each operand evaluation: We need to save argl and env, since (g 'x) might modify it

; d. (f (g 'x) 'y)

; Operator evaluation: None
; Operand sequence evaluation: Save proc since (g 'x) will modify it.
; Each operand evaluation: Even though (g 'x) might change env, 'y doesn't depend on it, so we don't need
;                          to save it. argl still needs saving.
