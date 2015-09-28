; Raising subtypes to supertypes

(define (int->rat int)
  (make-rat int 1))

(put 'raise 'int int->rat)


; Here we have a choice of either creating an abstraction
; for real numbers or simply using scheme's primitive. I
; chose to create the abstraction though `make-real`.
(define (rat->real rat)
  (make-real (/ (numer rat) (denom rat))))

(put 'raise 'rational rat->real)


(define (real->complex real)
  (make-from-real-imag real 0))

(put 'raise 'real real->complex)
