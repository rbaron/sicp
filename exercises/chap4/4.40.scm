; Optimizing the housing puzzle solution

; The first question was already answer in exercise 4.39:
; - All possible assignments: 5^5 = 3125
; - All assignments after distinct: 5^5 - 5^4 = 120

; Load this file and drop yourself in the REPL with:
; $ cat 4.40.scm - | mit-scheme

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

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5)))
    (require (not (= baker 5)))

    (let ((cooper (amb 1 2 3 4 5)))
      (require (not (= cooper 1)))

      (let ((fletcher (amb 1 2 3 4 5)))
        (require (not (= fletcher 5)))
        (require (not (= fletcher 1)))
        (require (not (= (abs (- fletcher cooper)) 1)))

        (let ((miller (amb 1 2 3 4 5)))
          (require (> miller cooper))
        (let ((smith (amb 1 2 3 4 5)))
          (require
           (distinct? (list baker cooper fletcher miller smith)))
          (require (not (= (abs (- smith fletcher)) 1)))
          (list (list 'baker baker)
                (list 'cooper cooper)
                (list 'fletcher fletcher)
                (list 'miller miller)
                (list 'smith smith))))))))

(multiple-dwelling)
; => ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))
