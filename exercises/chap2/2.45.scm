; This exercise asks us to generalize `up-split` and `right-split` in terms of
; a common `split` procedure.

; As noted in exercise 2.44, `up-split` and `right-split` differ only in
; where the smaller versions of itself will be placed relatively to the
; current painter.

; In the case of `up-split`, we have:
; (below painter (beside smaller smaller))

; In `right-split`:
; (beside painter (below smaller smaller))

; We can see that the only thing that's changing it the placement of
; the smaller copies between theirselves and between them and `painter`.

(define (split painter painter-to-smallers between-smallers n)
  (if (= n 0)
    painter
    (let ((smaller (split painter painter-to-smallers between-smallers (- n 1))))
      (painter-to-smallers painter (between-smallers smaller smaller)))))


; Of course we could come up with less crappy names for those arguments, but
; that's okay for now. Let's redefine `up-split` and `right-split`:

(define (up-split painter n)
  (split painter below beside n))

(define (right-split painter n)
  (split painter beside below n))
