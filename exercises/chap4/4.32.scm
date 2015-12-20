; Lazier streams

; To experiment in the lazy evaluator in a useful state,
; load this file with:
; $ cat 4.32.scm - | mit-scheme

; As pointed in the book, implementing `cons` as a procedure
; using our lazy evaluation mechanism yields even lazier streams
; than the one provided by Scheme itself.

(load "book-code/ch4-mceval.scm")
(load "book-code/ch4-leval.scm")

(define the-global-environment (setup-environment))
(driver-loop)

; Here's the procedural list implementation:

(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))

(define (add-inf-lists l1 l2)
  (cons (+ (car l1) (car l2))
        (add-inf-lists (cdr l1) (cdr l2))))

; This allows us to define recursive infinite streams
; such as:
(define ones (cons 1 ones))

(define integers (cons 1 (add-inf-lists ones integers)))

; A difference, as mentioned, between this implementation of streams
; and the original Scheme implementation, is that the `car` of the
; stream is _also_ lazy here. This means we could handle "loops" as
; the one in the `integral` procedure in a more straight-forward manner,
; since we do not need to explicitly call `delay` on specific arguments.
