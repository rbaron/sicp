; Enumerating all positive integers with no prime factors
; other than 2, 3 or 5.

; Given `merge` procedure:
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

; Analysis of `merge`:
; Read the head of two streams. Take the smallest one. If
; they're equal, take just one. This will automatically
; guarantee that the stream is in ascending order and that
; there are no duplicates.

; The exercise then asks us to complete the following skeleton
; to generate the proposed sequence:

; (define S (cons-stream 1 (merge <??> <??>)))

; Let's start by defining the constituting streams:

(define mul-2
  (cons-stream 1 (scale-stream mul-2 2)))

(define mul-3
  (cons-stream 1 (scale-stream mul-3 3)))

(define mul-5
  (cons-stream 1 (scale-stream mul-5 5)))

; Now all we have to do is merge all three streams
(define S (cons-stream 1 (merge mul-2
                                (merge mul-3 mul-5))))


; Testing

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (stream-take n stream)
  (if (= n 0)
    '()
    (cons (stream-car stream)
          (stream-take (- n 1) (stream-cdr stream)))))


(stream-take 20 S)
