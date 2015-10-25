; Let's analyze the creation os streams

; Suppose we'd want to implement streams from
; the ground up:

(define (cons-stream-custom a b)
  (cons a (delay-custom b)))

(define (car-stream-custom s)
  (car s))

(define (cdr-stream-custom s)
  (force-custom (cdr s)))

(define the-empty-stream-custom '())

(define (force-custom delayed-object)
  (delayed-object))

(define (delay-custom exp)
  (lambda () exp))

; Let's try it out with our lippy enumeration process:
(define (enumerate-stream-custom low high)
  (newline)
  (display "enum!")
  (if (= low high)
    the-empty-stream-custom
    (cons-stream-custom
      low
      (enumerate-stream-custom (+ low 1) high))))

(define stream1 (enumerate-stream-custom 0 10))

; Weird! Evaluating our supposedly lazy stream enumeration causes
; "enum!" to be printed 11 times! This means the custom stream
; implementation is not really lazy, since we're already evaluating
; every stream element!

; If you checkout footnote 56, the author says `cons-stream` must
; be a special form. If it was a procedure, as it is with every
; procedure, the arguments would be evaluated upon calling them!
; This would make the `cdr` part of the stream to be evaluated,
; which is precisely what we do not want. The same goes for `delay`.

; Let's try to do the same with the default stream implementation:

(define (enumerate-stream low high)
  (newline)
  (display "enum!")
  (if (= low high)
    the-empty-stream
    (cons-stream
      low
      (enumerate-stream (+ low 1) high))))

(define stream2 (enumerate-stream 0 10))

; Cool! This only causes "enum!" to be printed once. The second
; argument to the first `cons-stream` was never evaluated! This
; is the behavior we expected. This is why `cons-stream` and `delay`
; have to be special forms.
