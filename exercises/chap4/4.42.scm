; Load this file and drop yourself in the REPL with:
; $ cat 4.42.scm - | mit-scheme

(load "book-code/ch4-mceval.scm")
(load "book-code/ch4-ambeval.scm")

(define the-global-environment (setup-environment))
(driver-loop)

(define (require p)
  (if (not p) (amb)))

(define (xor clause1 clause2)
  (not (eq? clause1 clause2)))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (solutions)
  (let ((betty (amb 1 2 3 4 5))
        (ethel (amb 1 2 3 4 5))
        (joan  (amb 1 2 3 4 5))
        (kitty (amb 1 2 3 4 5))
        (mary  (amb 1 2 3 4 5)))

    (require (xor (= kitty 2)
                  (= betty 3)))

    (require (xor (= ethel 1)
                  (= joan 2)))

    (require (xor (= joan 3)
                  (= ethel 5)))

    (require (xor (= kitty 2)
                  (= mary 4)))

    (require (xor (= mary 4)
                  (= betty 1)))

    (require (distinct? (list betty ethel joan kitty mary)))

    (list betty ethel joan kitty mary)
))

(solutions)
; => (3 5 2 1 4)

try-again
; => There are no more values of (solutions)
