; Handling sums and products of arbitrary lenghts.

; Let's follow the tip and extend the contructors and selections of
; sums and products instead of altering the `deriv` procedure itself.
; For now, let's forget about stripping zeros and summing consecutive
; numbers:
(define (make-sum . exps)
  (cons '+ (sum-conseq-numbers (strip-zeros exps))))

; Dummy implementations for now
(define (sum-conseq-numbers lst) lst)
(define (strip-zeros lst) lst)

; Modified selectors
(define (addend s) (cadr s))

(define (augend s)
  (if (pair? (cdddr s))
    ; "unpack" argument list as positional arguments
    (apply make-sum (cddr s))
    (caddr s)))

; Testing
(define s1 (make-sum 2 '(+ 3 4) 5))
s1
; => (+ 2 (+ 3 4) 5)

(addend s1)
; => 2

(augend s1)
; => (+ (+ 3 4) 5)

; Analogously, for product:
(define (make-product . exps)
  (cons '* (mult-conseq-numbers (strip-ones exps))))

; Dummy implementations for now
(define (mult-conseq-numbers lst) lst)
(define (strip-ones lst) lst)

; Modified selectors
(define (multiplier s) (cadr s))

(define (multiplicand s)
  (if (pair? (cdddr s))
    ; "unpack" argument list as positional arguments
    (apply make-product (cddr s))
    (caddr s)))

; Testing
(define p1 (make-product 2 '(+ 3 4) 5))
p1
; => (* 2 (+ 3 4) 5)

(multiplier p1)
; => 2

(multiplicand p1)
; => (* (+ 3 4) 5))

(multiplicand (multiplicand p1))
; => 5

; Now we have our augmented data types. Let's see them in action.

; Given procedures
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

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

; Testing everything out:

; 1. Wth the old notation (note that no simplifications are made!)
(deriv '(* (* x y) (+ x 3)) 'x)
; => (+ (* (* x y) (+ 1 0)) (* (+ (* x 0) (* 1 y)) (+ x 3)))

; Simplifying by hand:
; => (+ (* x y) (* y (+ x 3)))

; 2. With the new notation
(deriv '(* x y (+ x 3)) 'x)
; => (+ (* x (+ (* y (+ 1 0)) (* 0 (+ x 3)))) (* 1 (* y (+ x 3))))

; Simplifying by hand:
; => (+ (* x y) (* y (+ x 3)))

; Same results! :)
