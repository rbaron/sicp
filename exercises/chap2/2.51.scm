; Let's define the `below` operation in two ways. It should receive two painters
; as arguments and return a new painter that places the two arguments vertically.

; 1. Defining `below` analogously to the given `beside` procedure:
; Places painter1 below painter2
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))


; 2. Defining `below` in terms of `beside^.
;   Idea: flip painter1 and painter2 of 90 degrees clockwise, call beside,
;   flip the result back.
(define (below painter1 painter2)
  (unflip-90
    (beside (flip-90 painter1)
            (flip-90 painter2))))
