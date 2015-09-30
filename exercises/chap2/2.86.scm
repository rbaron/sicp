; Complex numbers with rational real and imaginary parts.

; We'd have to allow constructions in the form of:
(define c1 (make-complex-from-real-imag
  (make-rat 1 2)
  (make-real 1.6)))

; This is already supported. Let's now look, inside the complex
; package, for functions that operate on complexes' real and
; imaginary parts.

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))


; We're gonna assume that basic mathematical operations such
; as `+`, `-`, `*` and `/` are already definied for every data
; type (they are defined as `add`, `sub`, `mul` and `div`).

; The solution is now for us the refactor the `real-part`, `imag-part`,
; `magnitude` and `angle` for both complex implementations: the rectangular
; and the polar one.

; Let's start with the rectangular one:

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z)) ; => This is ok

  (define (imag-part z) (cdr z)) ; => This is ok

  (define (make-from-real-imag x y) (cons x y)) ; => This is ok

  ; Assuming `(square x)` is defined as `(* x x)`,  this is ok
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))

  ; => `atan` needs to be defined for rationals too!
  (define (angle z)
    (apply-generic 'atan (imag-part z) (real-part z)))

  ; => `cos` and `sin` need to be defined for rationals too!
  (define (make-from-mag-ang r a)
    (cons (* r (apply-generic 'cos a)) (* r (apply-generic 'sin a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

; Let's fix those procedures.

; We can simply make real numbers out of rationals and call their original
; implementation
(define (rat-atan rat1 rat2)
  (atan (make-real (/ (numer rat1) (denom rat1)))
        (make-real (/ (numer rat2) (denom rat2)))))

(define (rat-sin rat
  (sin (make-real (/ (numer rat) (denom rat))))))

(define (rat-cos rat
  (cos (make-real (/ (numer rat) (denom rat))))))

(put 'atan 'rational rat-atan)
(put 'sin 'rational rat-sin)
(put 'cos 'rational rat-cos)

(put 'atan 'real atan)
(put 'sin 'real sin)
(put 'cos 'real cos)


; Let's now look at the polar package:

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (apply-generic 'cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (apply-generic 'sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (apply-generic 'atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

; Since we have already defined `atan`, `sin` and `cos` for genertic data,
; we just need to add `apply-generic` to the polar package. Done.
