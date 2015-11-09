; Solving RLC circuits

(define (RLC R L C dt il0 vc0)
  (define il (integral (delay dil) il0 dt))
  (define vc (integral (delay dvc) vc0 dt))
  (define dvc (scale-stream il (- (/ 1 C) )))
  (define dil (add-streams
      (scale-stream vc (/ 1 L))
      (scale-stream il (- (/ R L)))))
  (define result (stream-map cons vc il))
  result)

; ========================================
;    Begin copied code for testing
; ========================================
(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

(define (scale-stream stream factor)
  (stream-map (lambda (e) (* e factor)) stream))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

; ========================================
;    END OF copied code for testing
; ========================================

(define solution-stream (RLC 1 1 .2 .1 0 10))

(stream-head solution-stream 10)
; => ((10 . 0) (10 . 1.) (9.5 . 1.9) (8.55 . 2.66) (7.220000000000001 . 3.249)
;     (5.5955 . 3.6461) (3.77245 . 3.84104) (1.8519299999999999 . 3.834181)
;     (-.0651605000000004 . 3.6359559) (-1.8831384500000004 . 3.2658442599999997))
