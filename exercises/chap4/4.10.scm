; Giving Scheme a new syntax

; Let's reanalize a part of your implementation of `eval`

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)

        ; ... (other evaluation rules)

        ; Rule for evaluating `if` expressions
        ((if? exp) (eval-if exp env))

        ; ... (other evaluation rules)
       ))

; Inside the rule for evaluating `if` expressions,
; we can think of `exp` as being simply text waiting
; to be evaluated as code. In order to change the syntax
; of `if` expression, we don't have to change anything
; in the `eval` procedure. We can simply change the
; the predicates and selectors for `if` expressions.

; Let's make the new syntax be something weird like:

; << maybe predicate consequence alternative >>

; This would require us to write a parser for the language,
; which transforms a string into tokens to be evaluated.
; Let's void doing that for now and think as the if expression
; already being parse into a list of tokens, like we have been
; feeing `eval` so far.

(define (if? exp)
  (eq? (cadr exp) 'maybe)

; This also stays the same
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

; Let's modify the extractors and selectors

(define (if-predicate exp)
  (caddr exp))

(define (if-consequent exp)
  (cadddr exp))

(define (if-alternative exp)
  (car (cadddr exp)))
