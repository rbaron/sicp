; Why does flatten-stream use delay explicitly?

; What would be wrong in using it as:
(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave
       (stream-car stream)
       (flatten-stream (stream-cdr stream)))))

; As opposed to the actual implementation:
(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
       (stream-car stream)
       (delay (flatten-stream (stream-cdr stream))))))

; => Without the delay call, the second argument of interleave-delayed
; would be evaluated. It means we would in fact evaluate the stream-cdr
; of the stream _and_ the recursive call to flatten-stream. This would
; go against the purpose of using streams, since we would be evaluating
; the whole stream beforehand.
