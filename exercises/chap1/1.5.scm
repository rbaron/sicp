Procedure
=========

(define (p) (p))

(define (test x y)
  (if (= x 0)
    0
    y))

(test 0 (p))


; Analysis 1: applicative-order evaluation
; ----------------------------------------
;
; In this strategy, the arguments of a function are always evaluated. That is, an expression is evaluated by substituting the innermost expressions.
;
; (test 0 (p))
;
; => (if (= 0 0)
;       0
;       ( ??? ))
;
; => ??? is the result of evaluting (p)
;
; Here what happens is that (p) is evaluated, which results in a loop, since procedure (p) calls itself via recursion.
;
;
; Analysis 2: normal-order evaluation
; -----------------------------------
;
; In this strategy, the leftmost expressions are substituted first. That is, the arguments are not evaluated until they're needed.
;
; (test 0 (p))
;
; => (if (= 0 0)
;     0
;     (p))
;
; => 0
