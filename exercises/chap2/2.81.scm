; Same type coercion

; Louis Reasoner installed the following procedures:

(define (scheme-number->scheme-number n) n)

(define (complex->complex z) z)

(put-coercion 'scheme-number 'scheme-number
              scheme-number->scheme-number)

(put-coercion 'complex 'complex complex->complex)

; a.  What happens when `exp` is called with two `complex`s?

(define (exp x y) (apply-generic 'exp x y))

(put 'exp '(scheme-number scheme-number)
     (lambda (x y) (tag (expt x y)))) ; using primitive expt

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

; => When calling `exp` with two complex numbers, we'll enter an infinite loop.
; `apply-generic` will convert the first argument with `complex -> complex`,
; which will produce the first argument itself. As there is no entry for `exp` with
; arguments `'(complex complex)`, the system will re-call `apply-generic` with the
; exact same arguments as before, thus, entering an inifinite loop.

; b. Is Louis correct in assuming the procedure was broken?

; => Louis is wrong. Before installing the "coercion-to-self" procedure, the
; `apply-generic` procedure would try to look up the coercion and would throw
; an error, which is the expected behavior, since the procedure had already
; failing looking up the procedure for the given arguments.

; c. Modify `apply-generic` so it doens't try coercion for two arguments of the
; same type.

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (= type1 type2)
                  (error "There is no such procedure for this type -- " type1)

                  (let ((t1->t2 (get-coercion type1 type2))
                        (t2->t1 (get-coercion type2 type1)))
                    (cond (t1->t2
                           (apply-generic op (t1->t2 a1) a2))
                          (t2->t1
                           (apply-generic op a1 (t2->t1 a2)))
                          (else
                           (error "No method for these types"
                                  (list op type-tags))))))
                (error "No method for these types"
                       (list op type-tags))))))))


