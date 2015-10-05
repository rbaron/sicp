; Subtracting polynomials

; Inside the polynomials package, we could define:
(define (sub-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (sub-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- SUB-POLY"
             (list p1 p2))))

(define (sub-terms L1 L2)
  (add-terms L1 (negate-term-list L2)))

; Helper function: negate every term in term list
(define (negate-term-list term-list)
  (if (empty-termlist? term-list)
    term-list
    (let ((head (first-term term-list)))
      (adjoin-term
        (make-term (order head) (- (coeff head)))
        (negate-term-list (rest-terms term-list))))))

; Installing it
(put 'sub '(polynomial polynomial)
     (lambda (p1 p2) (tag (sub-poly p1 p2))))
