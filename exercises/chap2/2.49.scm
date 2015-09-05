; Let's implement a few painters using the `segments->painter`procedure.
; Remembering:

; - A painter is a procedure that receives a frame as argument and
;   draws something on the screen
; - `segments->painter` is a procedure that receives a `segment-list` and
;   returns a painter that draws those segments on the screen


; 1. A painter that draws the outline of the designated frame>

; The idea is to create a list of segments that wraps the unit square.
; When the produced painter is called with its designated frame, those
; segments will be translated and scaled to fit the frame.

(define outline-painter
  (segments->painter
    (list (make-segment (make-vect 0 0) (make-vect 0 1))
          (make-segment (make-vect 0 1) (make-vect 1 1))
          (make-segment (make-vect 1 1) (make-vect 1 0))
          (make-segment (make-vect 1 0) (make-vect 0 0)))))

; 2. A painter that draws an ``X'' by connecting opposite corners of the frame

(define x-painter
  (segments->painter
    (list (make-segment (make-vect 0 0) (make-vect 1 1))
          (make-segment (make-vect 0 1) (make-vect 1 0)))))

; 3. The painter that draws a diamond shape by connecting the midpoints of
;    the sides of the frame
(define diamond-painter
  (segments->painter
    (list (make-segment (make-vect 0 .5) (make-vect .5 1))
          (make-segment (make-vect .5 1) (make-vect 1 .5))
          (make-segment (make-vect 1 .5) (make-vect .5 0))
          (make-segment (make-vect .5 0) (make-vect 0 .5)))))

; 4. The `wave` painter

; This one is completely analogous to the first three cases, but has many more
; segments. Insteady of measuring and go trial and error, I decided to look up the
; coordinates from someone who has a little more patience than I do. I took them fro
; here [0].

(define wave
  (segments->painter
    (list (make-segment (make-vect 0.006 0.840) (make-vect 0.155 0.591))
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


; [0]: http://www.billthelizard.com/2011/10/sicp-249-defining-primitive-painters.html
