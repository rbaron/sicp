; In this exercise we are supposed to complete the implementation of the
; `horner-eval` procedure that evaluates a polynom based on Horner's rule.

; Given prodecure
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

; To-be-complete procedure
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)) )
              0
              coefficient-sequence))

; Testing
(horner-eval 2 (list 1 3 0 5 0 1))
; => 79
