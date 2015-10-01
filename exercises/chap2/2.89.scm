; term-list abstraction for dense polynomials.

; For dense polys, it makes sense to represent its terms with
; a simple list, ordered by decreasing term order.


; We can, for instance, modify _only_ how terms are stored, and
; retrieved. Upon extraction with `first-term`, the term itself
; will still be represented as it was before, via `make-term`.


; Fill term-list with zeroes until we can push the new term
(define (adjoin-term term term-list)
  (cond ((=zero? (coeff term)) term-list)
        ((empty-term-list? term-list)
          (cons (coeff term) term-list))
        (else (let ((term-order (order term))
                   ((highest-order (- (length term) 1)))
          (if (= term-order (+ highest-order 1))
            (cons (coeff term) term-list)
            (adjoin-term term
                         (cons 0 term-list))))))))


(define (first-term term-list)
  (make-term (- (length term-list) 1)
             (car term-list)))
