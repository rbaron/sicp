; Solving the house puzzle with the Amb evaluator

; Load this file and drop yourself in the REPL with:
; $ cat 4.38.scm - | mit-scheme

(load "book-code/ch4-mceval.scm")
(load "book-code/ch4-ambeval.scm")

; Kick off Amb-Eval REPL. From now on everything is from
; the user's perspective.
(define the-global-environment (setup-environment))
(driver-loop)

(define (require p)
  (if (not p) (amb)))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

; Let's omit the requirement that Smith and Fletcher do not
; live on adjacent floors:

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    ;(require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

(multiple-dwelling)
; => ((baker 1) (cooper 2) (fletcher 4) (miller 3) (smith 5))

try-again
; => ((baker 1) (cooper 2) (fletcher 4) (miller 5) (smith 3))

try-again
; => ((baker 1) (cooper 4) (fletcher 2) (miller 5) (smith 3))

try-again
; => ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))

try-again
; => ((baker 3) (cooper 4) (fletcher 2) (miller 5) (smith 1))

try-again
; => ;;; There are no more values of

try-again
; => (multiple-dwelling)

; There are 5 solutions.
