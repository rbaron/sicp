; Generic `=zero?` procedure. It should work on ordinary, rational and complex numbers.

; On ordinary numbers:
(define (install-scheme-number-package)
  ;...
  (put '=zero? '(scheme-number)
    (lambda (a) (= a 0)))
  'done)

; On rational numbers:
(define (install-rational-package)
  ;...
  (put '=zero? '(rational)
    (lambda (a) (= (numer a) 0)))
  'done)

; On complex numbers:
(define (install-rational-package)
  ;...
  (put 'equ? '(complex)
    (lambda (a)
      ; or simply (= (magnitude a) 0)
      (and (= (real-part a) 0)
           (= (imag-part a) 0))))
  'done)
