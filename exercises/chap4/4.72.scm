; Interleave vs. append for streams

; Load this file and drop yourself in the REPL with:
; $ cat 4.72.scm - | mit-scheme

; As we've seen in chapter 3, using iterleave makes sure that the resulting
; stream contains elements of both streams passed as argument. This is
; specially interesting for infinite arrays, because simply appending
; them would produce an inifite array with only elements form the first
; stream.

; As an example, let's make two infite streams:

(define ones (cons-stream 1 ones))
(define twos (cons-stream 2 twos))

(define (interleave s1 s2)
  (cons-stream (stream-car s1)
               (interleave s2 (stream-cdr s1))))

(stream-head (interleave ones twos) 10)
; => (1 2 1 2 1 2 1 2 1 2)
