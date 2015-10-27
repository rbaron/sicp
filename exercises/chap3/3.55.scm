; Partial sums of streams

; We should define a procedure that, given a stream `s`:

; S0, S1, S2, S3 , ...

; returns the stream:

; S0, S0 + S1, S0 + S1 + S2

(define (partial-sums stream)
  (cons-stream (stream-car stream)
               (stream-map
                  (lambda (el) (+ (stream-car stream) el))
                  (partial-sums (stream-cdr stream)))))

; Testing

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (stream-take n stream)
  (if (= n 0)
    '()
    (cons (stream-car stream)
          (stream-take (- n 1) (stream-cdr stream)))))

(stream-take 10 (partial-sums integers))
; => (1 3 6 10 15 21 28 36 45 55)
