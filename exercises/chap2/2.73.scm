; Data-directed symbolic differentiation

; a1. Explain what was done in the following modification:

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; => By modeling the `deriv` procedure with data-oriented design,
; we can "abstract" specific operations in table, as suggested on
; section 2.4.3. It makes it easier to add new types of expressions.
; By doing so, we have to create a table that looks like this:

;                       TYPE
;                    '+          '*
;              _______________________________
; OP  'deriv  |  deriv-sum  |  deriv-product  |
;              _______________________________
;
; a2. Why can't we assimilate the predicates `number?` and `same-variable?`
;     into the data-directed dispatch?
;
; => In both those cases, our selectors `operator` and `operands` would break,
; because numbers and variables are not lists.
;
;
; b. Writing procedures for derivatives of sums and products, as well as code
; to install them.

; Sum
(define (install-sum-package)
  ; Internal procedures
  (define (addend operands) (car operands))
  (define (augend operands) (cadr operands))

  (define (deriv-sum operands)
    (make-sum (deriv (addend operands))
              (deriv (augend operands))))

  (put 'deriv '+ deriv-sum))

; Product
(define (install-product-package)
  ; Internal procedures
  (define (multiplier operands) (car operands))
  (define (multiplicand operands) (cadr operands))

  (define (deriv-product operands)
    (make-sum
      (make-product (multiplier exp)
                    (deriv (multiplicand exp) var))
      (make-product (deriv (multiplier exp) var)
                    (multiplicand exp))))

  (put 'deriv '* deriv-product))


; c. Installing additional packages
(define (install-exponentiation-package)
  ; Internal procedures
  (define (base operands) (car operands))
  (define (exponent operands) (cadr operands))

  (define (deriv-exponentiation operands)
    (make-product
      (exponent operands)
      (make-product
        (make-exponentiation (base operands)
                             (- (exponent operands) 1))
        (deriv (exponent operands)))))

  (put 'deriv '** deriv-exponentiation))


; d. What if we indexed the procedures the other way around?

; => In this case, our table would be transposed, as in:

;                TYPE

;               'deriv
;          _________________
; OP  '*  |  deriv-product  |
;          _________________
;     '+  |    deriv-sum    |
;          _________________
;
; Since all we need was transposing the OP and TYPE lines on the matrix, the
; only change we'd need to do in our program is how packages are installed.
; That is, instead of having, for instance:
;
;  (put 'deriv '** deriv-exponentiation))
;
; we'd have:
;
;  (put '** 'deriv deriv-exponentiation))
