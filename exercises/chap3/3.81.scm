; Creating a resetable stream of prime numbers

; As the exercise asked, we should implement a stream of
; prime numbers without using variable assignments. The stream
; should be "resetable", which means we must be able to set the
; next stream element to a given number.

; Dummy `rand-update` implementation
(define (rand-update x) (modulo (expt x 17) 5231))

(define (make-rand-gen random-init msg-stream)

  (define (gen last-rand msg-stream)
    (if (stream-null? msg-stream)
      the-empty-stream
      (let ((msg (stream-car msg-stream)))
        (cond ((eq? msg 'generate)
                (let ((new-rand (rand-update last-rand)))
                  (cons-stream new-rand (gen new-rand (stream-cdr msg-stream)))))
              ((eq? msg 'reset)
                  (make-rand-gen random-init (stream-cdr msg-stream)))))))

  (gen random-init msg-stream))

; Testing
(define msg-stream (list->stream (list
  'generate
  'generate
  'generate
  'reset
  'generate
  'generate
  'generate)))

(define rand-stream (make-rand-gen 11 msg-stream))

rand-stream

(stream-head rand-stream 6)
; => (3568 2171 273 3568 2171 273)
