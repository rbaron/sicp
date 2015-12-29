; Faster nondeterministic solution for the housing puzzle

; Does the order of the restrictions affect the efficiency
; of searching a solution?

; Yes! Even though, from the programmers point of view, all solutions
; are evaluated "at the same time", in reality, our Amb evaluator
; does a depth-first search in the solution space.

; In order to come up with a faster approach, we can levarage two
; ideas:
; 1. Operations that run more often should be cheaper
; 2. Restrictive operations should come first - closer to the root
;    of the tree, since the DFS will backtrack earlier and avoid
;    wasted computation on invalid solutions.

; Given those two points, we can evaluate each of the `require`
; expressions and assign "cost" and "how restrictive?" measures
; to it.

; To get an intution, here's how one could proceed:

; - The whole search space has 5^5 = 3125  possible solutions
; - The first clause will be evaluated every time, so we might
;   want to put the most restrictive solution there.
;   The `distinct?` requirement seems to be restrictive enough,
;   since it will only be satisfied in 5*4*3*2*1 = 5! = 120 cases;
;   It seems to be a relatively expensive computation, though.
; - The requirements for not living on a given level are not very
;   restrictive. 5^5 - 5^4 = 2500 solutions satisfy each.
; - Miller lives higher than cooper: By symmetry:
;   n = (5^5 - 5^4)/2 = 1250 combinations

; Below there is a reordering of the statements taking into
; consideration these observations.

; Load this file and drop yourself in the REPL with:
; $ cat 4.39.scm - | mit-scheme

(load "book-code/ch4-mceval.scm")
(load "book-code/ch4-ambeval.scm")

(define the-global-environment (setup-environment))
(driver-loop)

(define (require p)
  (if (not p) (amb)))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

; Original
;(define (multiple-dwelling)
;  (let ((baker (amb 1 2 3 4 5))
;        (cooper (amb 1 2 3 4 5))
;        (fletcher (amb 1 2 3 4 5))
;        (miller (amb 1 2 3 4 5))
;        (smith (amb 1 2 3 4 5)))
;    (require
;     (distinct? (list baker cooper fletcher miller smith)))
;    (require (not (= baker 5)))
;    (require (not (= cooper 1)))
;    (require (not (= fletcher 5)))
;    (require (not (= fletcher 1)))
;    (require (> miller cooper))
;    (require (not (= (abs (- smith fletcher)) 1)))
;    (require (not (= (abs (- fletcher cooper)) 1)))
;    (list (list 'baker baker)
;          (list 'cooper cooper)
;          (list 'fletcher fletcher)
;          (list 'miller miller)
;          (list 'smith smith))))

; Reordered
(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper fletcher miller smith)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

(multiple-dwelling)

