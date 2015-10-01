; `=zero?` for polynomials.

; Let's define the `=zero?` procedure for polynomials. A polynomial
; is zero if it's either empty or all coefficients are zero

(define (=zero?-term-list term-list)
  (cond ((empty-termlist? term-list) #t)
        ((= (coeff (first-term term-list)) 0)
          (=zero?-term-list (rest-terms term-list)))
        (else #f)))


(define (=zero?-poly poly)
  (=zero?-term-list (term-list poly)))


(put '=zero? 'polynomial =zero?-poly)
