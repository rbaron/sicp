; From prefix to postfix notation: let's modify our symbolic derivation
; framework to deal with infix operands.

; a. With balanced parenthesis
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

; Testing
(addend (make-sum 1 '(2 * 3)))
; => 1

(augend (make-sum 1 '(2 * 3)))
; => (2 * 3)

(multiplier (make-product 2 '(3 + 4)))
; => 2

(multiplicand (make-product 2 '(3 + 4)))
; => (3 + 4)

(deriv '((x * y) * (x + 3)) 'x)
; => ((x * y) + (y * (x + 3)))

(define exp0 '(x + (3 * (x + y + 2))))
(deriv exp0 'x)

; b. With standard algebraic notation (dropping unecessary parenthesis)

; The idea is to allow expressions of the type:

; (x + 3 * (x + y + 2))

; which may have supressed unecessary parenthesis.
; One possible way to takle this problem is to modify our selectors,
; predicates and constructors.

; We may notice that now we may represent the "second" terms of sum
; and product differently (expressions are now lists with arbitrary
; length, instead of (el1 operator el2). This suggest we need to modify
; the selectors of these "seconds terms":

; In addition, the second term is the `augend`:
(define (augend expr)
  (let ((aug (cddr expr)))
    (if (= (length aug) 1)
      (car aug)
      aug)))

; In product, the second term is the `multiplicand`:
(define (multiplicand expr)
  (let ((mul (cddr expr)))
    (if (= (length mul) 1) (car mul) mul)))

; Testing:
(define exp1 '(x + 3 * (x + y + 2)))
(deriv exp1 'x)
; => 4
