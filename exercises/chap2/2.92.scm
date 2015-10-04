; Operating on polynomials with different variable ordering

; For instance, we'd like to be able to add the two following polys:

; p1 => (y + 2)*x^2 + (y^3 + y)*x^1 + (y^2)*x^0 = 0
; p2 => (x)*y^2 + (x^3)*y^1 + (x^3 + 2*x)*y^0 = 0

; The idea is to define a natural ordering for the polys' variables
; so that we can transform polynomials to be in a "compatible" order.
; In this example, we would like to transform p2 so that it becomes:


; p2 => (x)*y^2 + (x^3)*y^1 + (x^3 + 2*x)*y^0 = 0
;    => (y + 1)*x^3 + (0)*x^2 + (y^2 + 2)x^1 + (0)*x^0 = 0
;
;
; Now both p1 and p2 are defined with `x` as their "top level" variable.

; For simplicity, let's assume initially polys have at most two levels of
; data recursion. That is, a poly's terms might be polys themselves, but
; it only goes one level deep.

; `invert-expand-terms`:
; For every sub-polynomial-term from the original polynomial,
; produce a list of interted single-coefficient polynomials.
; Example:
;
; let p2 = (x^2 + 2x + 1)*y^2 + (x^3)*y^1 + (x^3 + 2*x)*y^0 = 0
;
; The first sub-polynomial-term is (x^2 + 2x + 1). It's order is 2
; and variable is y.
; It will be expanded to:
;
; OUPUT = '( (y^2)*x^2 (2*y^2)*x^1 (y^2)*x^0 )
(define (invert-expand-terms poly-coeff orig-var orig-order)
  (map
    (lambda (term)
      (make-poly (variable poly-coeff)
                 (list (list (order term)
                             (make-poly orig-var (list (list orig-order (coeff term))))))))
    (term-list poly-coeff)))

; For simplicity, I'm gonna assume _every_ term of the original
; polynomial is a polynomial itself.
(define (expand-invert-all-terms poly)
  (flat-map
    (lambda (poly-term)
      (invert-expand-terms (coeff poly-term) (variable poly) (order poly-term)))
    (term-list poly)))

(define (invert poly)
  (reduce-left add-poly '() (expand-invert-all-terms poly)))

(define (flat-map func lst)
  (if (null? lst)
    '()
    (append (func (car lst)) (flat-map func (cdr lst)))))

; Given procedures with some exercise-specific modification (AKA.
; dirty hacks) where commented
(define (make-poly variable term-list)
    (cons variable term-list))

(define (variable p) (car p))

(define (term-list p) (cdr p))

(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1)) (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1 (add-terms (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   t2 (add-terms L1 (rest-terms L2))))
                 (else
                  (adjoin-term
                   (make-term (order t1)
                              ; Super dirty hack to avoid installing multiple
                              ; `add` operations. Since we're only using numbers
                              ; and polynomials in this exercise specifically... ;P
                              (if (pair? (coeff t1))
                                (add-poly (coeff t1) (coeff t2))
                                (+ (coeff t1) (coeff t2))))
                   (add-terms (rest-terms L1)
                              (rest-terms L2)))))))))

(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))

(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
         (make-term (+ (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms t1 (rest-terms L))))))

(define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))
(define (mul-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- MUL-POLY"
             (list p1 p2))))

(define (adjoin-term term term-list)
  ;(if (=zero? (coeff term))
  ;    term-list
  ;    (cons term term-list)))
  ; Quick hack for letting us not worry too much about our "type system"
  ; during this exercise. Otherwise we'd have to install `=zero?` procedures
  ; for at least two data types - numbers and polys
  (cons term term-list))

(define (the-empty-termlist) '())

(define (first-term term-list) (car term-list))

(define (rest-terms term-list) (cdr term-list))

(define (empty-termlist? term-list) (null? term-list))

(define (make-term order coeff) (list order coeff))

(define (order term) (car term))

(define (coeff term) (cadr term))

(define (=zero?-term-list term-list)
  (cond ((empty-termlist? term-list) #t)
        ((= (coeff (first-term term-list)) 0)
          (=zero?-term-list (rest-terms term-list)))
        (else #f)))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

; ENDOF given procedures

; Let's define some nested polynomials to test our program on:

; p1 => (y + 2)*x^2 + (2*y^3 + y)*x^1 + (y^2)*x^0 = 0
(define p1-termlist
  (adjoin-term
    (make-term 2
               (make-poly 'y '((1 1) (0 2))))
    (adjoin-term
      (make-term 1
                 (make-poly 'y '((3 2) (1 1))))
      (adjoin-term
        (make-term 0
                   (make-poly 'y '((2 1))))
      (the-empty-termlist)))))

(define p1 (make-poly 'x p1-termlist))

; p2 => (2*x^2 + x + 3)*y^2 + (x^3)*y^1 + (x^3 + 2*x)*y^0 = 0
(define p2-termlist
  (adjoin-term
    (make-term 2
               (make-poly 'x '((2 2) (1 1) (0 3))))
    (adjoin-term
      (make-term 1
                 (make-poly 'x '((3 1))))
      (adjoin-term
        (make-term 0
                   (make-poly 'x '((3 1) (1 2))))
      (the-empty-termlist)))))

(define p2 (make-poly 'y p2-termlist))

; Testing

(invert p2)
; => (x (3 (y (1 1) (0 1))) (2 (y (2 2))) (1 (y (2 1) (0 2))) (0 (y (2 3))))

; Which translates to:
; => (y + 1)*x^3 + (2*y^2)*x^2  + (y^2 + 2)*x^1 + (3*y^2)*x^0

; Success!

; Now, finally, let's test our idea.

; Since `p1` and `p2` are polynomials on different variables,
; this should raise an error:
(add-poly p1 p2)
; => ;Polys not in same var -- ADD-POLY ((x ... ... ...) (y ... ... ...))

; And this should produce the expected result:
(add-poly p1 (invert p2))
; => (x (3 (y (1 1) (0 1))) (2 (y (2 2) (1 1) (0 2))) (1 (y (3 2) (2 1) (1 1) (0 2))) (0 (y (2 4))))

; Which translates to:
; => (y + 1)*x^3 + (2*y^2 + y + 2)*x^2 + (2*y^3 + y^2 + y + 2)*x^1 + (4*y^2)*x^0

; Where, again:
; p1 =>                        (y + 2)*x^2 + (2*y^3 + y)*x^1 + (y^2)*x^0 = 0
; (invert p2) => (y + 1)*x^3 + (2*y^2)*x^2  + (y^2 + 2)*x^1 + (3*y^2)*x^0

; Success! We got the expected result.

; This solution is merely a proof of concept. To make the program robust, we would
; still have to:
; - Insert this logic into `add-poly`, accounting for edge cases;
; - Work with arbitrarily deep "data recursion". That is, a poly's coeff's poly's coeff
;   could also be a poly (!);
; - Define an explicit ordering for variables. Here we just assumed 'x < 'y;
; - Deal with the fact that not _every_ term must be a polynomial. Some can be
;   numbers, rationals, complex numbers.
