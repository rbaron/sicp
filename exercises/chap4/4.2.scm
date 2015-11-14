; Rearanging the order of `cond` clauses in `eval`

; a. Louis' plan of s imply putting the clause `application?`
; before `assignment?` will not work. The reason is that
; `application?` is implemented as:

(define (application? exp) (pair? exp))

; Which means any expression that is a pair will evaluate
; `application?` to true.


; b. In order to support evaluating the clause for `application?`
; earlier, we would have to modify the predicate to be:

(define (application? exp)
  (tagged-list? exp 'call))

; This means we should also modify our selectors for application.
; Note, though, that we do not have to modify `eval` itself!

(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
