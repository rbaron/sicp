; Polynomial division

; 1. Let's complete the implementation of `div-terms`:

; `div-terms`:
; INPUT: term lists from the dividend and divisor
; OUPUT: resulting term list and remainder
(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let ((new-c (div (coeff t1) (coeff t2)))
                  (new-o (- (order t1) (order t2))))
              (let ((rest-of-result
                     ;; <compute rest of result recursively>
                     (div-terms
                        (sub-terms L1
                                   (mul-term-by-all-terms (make-term new-o new-c) L2))
                        L2)
                     ))
                ;;<form complete result>
                (list (adjoin-term (make-term new-o new-c) (car rest-of-result))
                      (cadr rest-of-result))))))))

; 2. Let's define the higher level `div-poly` procedure
(define (div-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (let ((variable (variable p1))
            (result (div-terms (term-list p1) (term-list p2))))
        (list (make-poly variable (car result))
              (make-poly variable (cadr result))))
      (error "Polys not in same var -- DIV-POLY"
             (list p1 p2))))
