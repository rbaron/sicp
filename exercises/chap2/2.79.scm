; Generic `equ?` procedure. It should work on ordinary, rational and complex numbers.

; On ordinary numbers:
(define (install-scheme-number-package)
  ;...
  (put 'equ? '(scheme-number scheme-number)
    (lambda (a b) (= a b)))
  'done)

; On rational numbers:
(define (install-rational-package)
  ;...
  (put 'equ? '(rational rational)
    (lambda (a b)
      (and (= (numer a) (= (number b)))
           (= (denom a) (= (denom b))))))
  'done)

; On complex numbers:
(define (install-rational-package)
  ;...
  (put 'equ? '(complex complex)
    (lambda (a b)
      (and (= (real-part a) (= (real-part b)))
           (= (imag-part a) (= (imag-part b))))))
  'done)
