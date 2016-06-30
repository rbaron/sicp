; Evaluating cond as a special form

; Instead of adopting the strategy of syntatically transforming
; cond expressions into in expressions (like we did in exercise 5.23),
; we are asked to handle cond expressions directly in the evaluator.

; Again, in order to dispatch the cond evaluations, we need to add the
; following line in the eval-dispatch code segment:

(test (op cond?) (reg exp))
(branch (label ev-cond))

; And, under the label ev-cond, we start the handling of cond expressions.
; We save the contents of continue and save and assign the clauses to the
; unev register.

ev-cond
  (save continue)
  (save env)
  (assign unev (op cond-clauses) (reg exp))
  (goto (label ev-cond-clauses))

; We now loop over all clauses in the cond statement until we find one whose
; predicate is true. When that happens, we want to setup the evaluation of
; that action and go to the main eval-dispatch section.

ev-cond-clauses
  (test (op no-more-clauses?) (reg unev))
  (branch (label ev-cond-no-mo-clauses))

  ; Select the first clause
  (assign exp (op cond-first-clause) (reg unev))

  ; Setup the evaluation of the predicate
  (save exp)
  (save continue)
  (assign exp (op cond-predicate) (reg exp))
  (assign continue (label ev-cond-after-predicate))
  (goto (label eval-dispatch))

; Here the register val holds the evaluated predicate and
; we have continue and the whole clause on the stack.
; unev holds the list of clauses (including the current)
ev-cond-after-predicate
  (restore continue)
  (restore exp)
  (test (op true?) val)
  (branch (label ev-cond-pred-true))

  ; If the predicate is not true, we recurse to ev-cond-clauses
  ; with the rest of the clauses in the unev register
  (assign unev (op cdr) (reg unev))
  (goto (label ev-cond-clauses))


; If the predicate of the clause is true, we restore the original
; env and continue and jump to the evaluation of the sequence
; formed by the actions of that clause. The whole clause is in the
; exp register.
ev-cond-pred-true
  (restore env)
  (restore continue)
  (assign exp (op cond-actions) (reg exp))
  (goto (label ev-sequence))

; If we run out of clauses, we setup the return value of false (assign
; (const false) to the val register) and jump to the continue pointer,
; after popping the saved info out of the stack
env-cond-no-mo-clauses
  (restore env)
  (restore continue)
  (assign val (const false))
  (goto continue)
