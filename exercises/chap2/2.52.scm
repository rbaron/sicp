; In this exercise we should make changes in different "layers" of the
; picture language framework, in order to illustrate the robustness of
; stratified design.

; a. Adding some segments to the primitive `wave`.
(define wave
  (segments->painter
    (list (make-segment (make-vect 0.006 0.840) (make-vect 0.155 0.591))

          ; New segment (a diagonal line)
          (make-segment (make-vect 0.0 0.0) (make-vect 1.0 1.0))

          (make-segment (make-vect 0.006 0.635) (make-vect 0.155 0.392))
          (make-segment (make-vect 0.304 0.646) (make-vect 0.155 0.591))
          (make-segment (make-vect 0.298 0.591) (make-vect 0.155 0.392))
          (make-segment (make-vect 0.304 0.646) (make-vect 0.403 0.646))
          (make-segment (make-vect 0.298 0.591) (make-vect 0.354 0.492))
          (make-segment (make-vect 0.403 0.646) (make-vect 0.348 0.845))
          (make-segment (make-vect 0.354 0.492) (make-vect 0.249 0.000))
          (make-segment (make-vect 0.403 0.000) (make-vect 0.502 0.293))
          (make-segment (make-vect 0.502 0.293) (make-vect 0.602 0.000))
          (make-segment (make-vect 0.348 0.845) (make-vect 0.403 0.999))
          (make-segment (make-vect 0.602 0.999) (make-vect 0.652 0.845))
          (make-segment (make-vect 0.652 0.845) (make-vect 0.602 0.646))
          (make-segment (make-vect 0.602 0.646) (make-vect 0.751 0.646))
          (make-segment (make-vect 0.751 0.646) (make-vect 0.999 0.343))
          (make-segment (make-vect 0.751 0.000) (make-vect 0.597 0.442))
          (make-segment (make-vect 0.597 0.442) (make-vect 0.999 0.144))))

; b. Changing the pattern contructured by `corner-split`
; by using only one painter on the right and on the top
; Idea: Simply remove calls to `beside` and `below` and use
; `up` and `right` themselves.
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
            (corner (corner-split painter (- n 1)))
          (beside (below painter up)
                  (below right corner))))))

; c. Modifying `square-limit` so that Mr. Rogers look outward from each
; corner of the square
; Idea: Flip the recursive call to `corner-split`
(define (square-limit painter n)
  (let ((quarter (flip-horiz (corner-split painter n))))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))
