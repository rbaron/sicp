; Turing's halting problem

(define (run-forever) (run-forever))

(define (try p)
  (if (halts? p p)
      (run-forever)
      'halted))

; Let's try to show by contradiction that no such procedure
; `halts?` can exist.

; Let's now analyze
(try try)

; 1. If (halts? try try) halts, we'll call (run-forever), which means
;    (try try) won't halt;
;
; 2. If (try try) won't halt, we'll return 'halt (which means the program
;    halts).

; This means it is impossible to write such a procedure `halts?`!
