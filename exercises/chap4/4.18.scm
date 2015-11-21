; Alternative strategy for scanning out definitions

; Original procedure
; (lambda <vars>
;   (define u <e1>)
;   (define v <e2>)
;   <e3>)

; Alternative strategy
; (lambda <vars>
;   (let ((u '*unassigned*)
;         (v '*unassigned*))
;     (let ((a <e1>)
;           (b <e2>))
;       (set! u a)
;       (set! v b))
;     <e3>))

; Let's analyze what will happen with the following
; call:

;(define (solve f y0 dt)
;  (define y (integral (delay dy) y0 dt))
;  (define dy (stream-map f y))
;  y)

; it will be converted to:
; (solve f y0 dt
;   (let ((y '*unassigned*)
;         (dy '*unassigned*))
;     (let ((a (integral (delay dy) y0 dt))
;           (b (stream-map f y)))
;       (set! y a)
;       (set! dy b))
;     y))

; It won't work, since `a` and `b` will be evaluated at
; the same time. When `b` is evaluated, `y` is still
; `'*unassigned*`.

; With the original transfomation, we would have:

; (solve f y0 dt
;   (let ((y '*unassigned*)
;         (dy '*unassigned*))
;     (set! y (integral (delay dy) y0 dt))
;     (set! dy (stream-map f y))
;     y))
