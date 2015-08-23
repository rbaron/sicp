; In this exercise, we are supposed to write a procedure `for-each`, which
; applies a given procedure to a list and simply returns true.

(define (for-each proc items)
  (cond ((null? items) #t)
  (else (proc (car items))
        (for-each proc (cdr items)))))

; Testing it out:
(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

; Produces
; 88
; 321
; 57
; Value: #t
