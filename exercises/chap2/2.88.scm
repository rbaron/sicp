; Subtracting polynomials

; Inside the polynomials package, we could define:

(define (sub-poly p1 p2)
  (add-poly p1 (negate-poly p2)))

; Helper function: negate every term in term list
(define (negate-term-list term-list)
  (if (empty-termlist? term-list)
    term-list
    (let ((head (first-term term-list)))
      (adjoin-term
        (make-term (order first-term) (- (coeff first-term)))
        (negate (rest-terms term-list))))))

(define (negate-poly poly)
  (make-poly (variable poly)
             (negate-term-list (term-list poly))))


(put 'sub 'polynomial sub-poly)
