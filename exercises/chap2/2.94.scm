; Rational functions - GCD for polynomials

; Here's how `gcd` works for integers:
; (define (gcd a b)
;   (if (= b 0)
;       a
;       (gcd b (remainder a b))))

; And the polynomial version:
; (define (gcd-terms a b)
;   (if (empty-termlist? b)
;       a
;       (gcd-terms b (remainder-terms a b))))

; Since our `div-poly` procedure already returns both the quotient
; and the remainder, the procedure `remainder-terms` should only
; be `(cadr (div-poly p1 p2))`.

; Let's install it inside our package so we can test it:

(define (install-rational-package)
  (define (make-rat n d)
    (let ((g (gcd-gen n d)))
      (cons (div n g) (div d g))))

  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (=zero?-rat x)
    (= (numer x) 0))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))

  (define (tag x) (attach-tag 'rational x))
  (put '=zero 'rational =zero?-rat)
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (install-polynomial-package)
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  (define (=zero-poly? poly)
    (equal? (term-list poly) the-empty-termlist))

  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  (define (adjoin-term term term-list)
    ;(cons term term-list))
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))

  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (sub-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- SUB-POLY"
               (list p1 p2))))

  (define (sub-terms L1 L2)
    (add-terms L1 (negate-term-list L2)))

  (define (negate-term-list term-list)
    (if (empty-termlist? term-list)
      term-list
      (let ((head (first-term term-list)))
        (adjoin-term
          (make-term (order head) (- (coeff head)))
          (negate-term-list (rest-terms term-list))))))

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((variable (variable p1))
              (result (div-terms (term-list p1) (term-list p2))))
          (list (make-poly variable (car result))
                (make-poly variable (cadr result))))
        (error "Polys not in same var -- DIV-POLY"
               (list p1 p2))))

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

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))

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
                                (add (coeff t1) (coeff t2)))
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

  (define (remainder-terms L1 L2)
    (cadr (div-terms L1 L2)))

  (define (gcd-terms a b)
    (if (empty-termlist? b)
      a
      (gcd-terms b (remainder-terms a b))))

  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (gcd-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- GCD-POLY"
             (list p1 p2))))

  (define (tag p) (attach-tag 'polynomial p))
  (put '=zero? 'polynomial =zero-poly?)
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial)
     (lambda (p1 p2)
       (let ((result (div-poly p1 p2)))
         (list (tag (car result))
               (tag (cadr result))))))
  (put 'gcd '(polynomial polynomial)
       (lambda (p1 p2) (tag (gcd-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put '=zero? '(scheme-number)
    (lambda (x) (= x 0)))
  (put 'gcd '(scheme-number) gcd)
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((symbol? datum) 'symbol)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (attach-tag type-tag contents)
  (cond ((number? contents) contents)
        ((symbol? contents) contents)
        (else (cons type-tag contents))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((symbol? datum) datum)
        (else (cdr datum))))

(define put 2d-put!)

; Since `2d-get` doesn't support a list of keys as key...
(define (get x-key y-key)
  (let ((1d-table (2d-get-alist-x x-key)))
    (let ((type-f (assoc y-key 1d-table)))
      (if type-f (cdr type-f) false))))

(install-polynomial-package)
(install-rational-package)
(install-scheme-number-package)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))

          ; Let's not worry about coercion in this exercise.
          ;(if (= (length args) 2)
          ;    (let ((type1 (car type-tags))
          ;          (type2 (cadr type-tags))
          ;          (a1 (car args))
          ;          (a2 (cadr args)))
          ;      (let ((t1->t2 (get-coercion type1 type2))
          ;            (t2->t1 (get-coercion type2 type1)))
          ;        (cond (t1->t2
          ;               (apply-generic op (t1->t2 a1) a2))
          ;              (t2->t1
          ;               (apply-generic op a1 (t2->t1 a2)))
          ;              (else
          ;               (error "No method for these types"
          ;                      (list op type-tags))))))
          (error "No method for these types" type-tags
                 (list op type-tags))))))

(define (=zero? x) (apply-generic '=zero? x))
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (greatest-common-divisor x y) (apply-generic 'gcd x y))

; Exposing the constructors
(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))


; Trying out the greatest-common-divisor
(define p1 (make-polynomial 'x '((4 1)(3 -1)(2 -2)(1 2))))
; => x4 - x3 - 2x2 + 2

(define p2 (make-polynomial 'x '((3 1)(1 -1))))
; => x3 - x

(greatest-common-divisor p1 p2)
; => (polynomial x (2 -1) (1 1))
; => -x2 + x
