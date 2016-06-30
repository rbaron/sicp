; Handling cond and let expressions in the evaluator

; a. cond

; Inside eval-dispatch code section, we can add:

(test (op cond?) (reg exp))
(branch (label ev-cond))

; We can then create an entry point in the REPL

ev-cond
  (assign exp (op cond->if) (reg exp))
  (goto (label eval-dispatch))

; b. let

; Analogously:

(test (op let?) (reg exp))
(branch (label ev-let))

ev-let
  (assign exp (op let->combination) (reg exp))
  (goto (label eval-dispatch))
