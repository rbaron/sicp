; Let's define some transformations on painters.

; 1. Flipping a painter horizontally

; Idea: we set the origin to (1 0), point x-axis back to (0 0) and point
; y-axis up to (1 1)
(define (flip-horiz painter)
  (transform-painter painter
     (make-vect 1 0)
     (make-vect 0 0)
     (make-vect 1 1)))

; 2. Rotating 180 degrees counterclockwise

; Idea: new origin is at (1 1), x-axis points left to (0 1), y-axis
; points down to (1 0)
(define (rot-180 painter)
  (transform-painter painter
     (make-vect 1 1)
     (make-vect 0 1)
     (make-vect 1 0)))

; 3. Rotating 270 degrees counterclockwise == rotating 90 clockwise

; Idea: new origin is at (0 1), x-axis points down to (0 0), y-axis
; points right to (1 1)
(define (rot-270 painter)
  (transform-painter painter
     (make-vect 0 1)
     (make-vect 0 0)
     (make-vect 1 1)))
