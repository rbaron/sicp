; Stream of factorials using `mul-streams`

; Let's implement `mul-streams`
(define (mul-streams s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (mul-streams (stream-cdr s1) (stream-cdr s2))))


; Now, using `mul-streams`, we can complete the following skeleton:

; (define factorials (cons-stream 1 (mul-streams <??> <??>)))

(define factorials
  (cons-stream 1
               (mul-streams factorials
                            integers)))

; See 2.54.jpg to see how the recursion tree would look like!

; Testing

(define integers (cons-stream 1 (add-streams ones integers)))

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (stream-take n stream)
  (if (= n 0)
    '()
    (cons (stream-car stream)
          (stream-take (- n 1) (stream-cdr stream)))))

(stream-take 10 factorials)
; => (1 1 2 6 24 120 720 5040 40320 362880)

; To be fully compliant with the indexing inside the stream,
; we can substitute `integers` for `integers-starting-from`:

(define (integers-starting-from n)
  (cons-stream n (add-streams ones (integers-starting-from n))))

(define t (integers-starting-from 20))

(define factorials
  (cons-stream 1
               (mul-streams factorials
                            (integers-starting-from 2))))

(stream-take 10 factorials)
; => (1 2 6 24 120 720 5040 40320 362880 3628800)
