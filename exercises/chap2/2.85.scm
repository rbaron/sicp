; Simplifying types.

; Let's follow the suggested path for simplifying types:
; A type can be simplified whenever we can `project` it to a lower
; type and, if we `raise` that, we get the original data back.

; Let's start by implementing and installing all the `project`
; procedures.

(define (project-complex n)
  (make-real (real-part n)))

(put 'project 'complex project-complex)


; Let's take advantage of scheme's builtin rational implementation:
; `inexact->exact x`   -> returns the closest rational number
;                         to the real number argument
; `numerator`           -> scheme's rational numerator
; `denominator`         -> scheme's rational denominator
(define (project-real n)
  (let ((rat (inexact->exact n)))
    (make-rat (numerator rat) (denominator rat))))

(put 'project 'real project-complex)


(define (project-rat n)
  (make-int (round (/ (numer n) (denom n)))))

(put 'project 'rational project-rat)


(define (project n)
  (apply-generic 'project n))

(define (raise n)
  (apply-generic 'raise n))


; The `drop` procedure simplifies its argument to the lowest
; possible type
(define (drop n)
  (if (or (= (type-tag n) 'integer)
          (not (can-drop? n)))
    n
    (drop (project n))))

(define (can-drop? n)
  (equ? n (raise (project n))))


; Modifying `apply-generic` (I picked the original version from the
; book over the one we modified in previous exercises.

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          ; Let's change this:
          ;(apply proc (map contents args))
          ; To this:
          (drop (apply proc (map contents args)))
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
