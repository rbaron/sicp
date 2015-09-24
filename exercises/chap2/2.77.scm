; Two-level type tagging

; Loius is trying to evaluate the following expression:
(magnitude z)

; Where z is:
(define z (cons 'complex (cons 'rectangular (cons 3 4))))

; The `magnitude` procedures are part of the rectangular and polar
; package for representing complex numbers. These procedures are
; installed as entries on the operation-vs-type-tag table, such as:


;                               TYPE
;                     'rectangular      'polar
;                    _______________________________
; OP  'magnitude    |  rect-mag  |  polar-mag      |
;                    _______________________________

; The called `magnitude` procedure is defined as:
(define (magnitude z) (apply-generic 'magnitude z))

; `apply-generic`, in its turn, gets `z`'s tag through the
; `type-tag` procedure, defined as:
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

; This means that the considered type tag for the lookup is
; the outmost tag. In this case, that tag is `complex`, which,
; indeed, is not present on the table.A

; As Alyssa suggests, installing the `'magnitude` procedure for
; the complex type tag as in
(put 'magnitude '(complex) magnitude)

; causes a new entry to be added on the lookup table. It'll look like:

;                               TYPE
;                     'rectangular      'polar     'complex
;                    __________________________________________
; OP  'magnitude    |  rect-mag  |  polar-mag   |  magnitude  |
;                    __________________________________________

; Now, when calling `magnitude` on `z`, the table is looked up once for the
; '(complex) tag, upon which the same `magnitude` procedure is returned.
; Then, when the procedure is looked up for the second time, the type tag
; will be `rectangular`, and the returned procedure will be `rect-mag`.
