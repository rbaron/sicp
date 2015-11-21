; Analyzing enviroments of the original and transformed
; expression

; a. Original version

; (lambda <vars>
;   (define u <e1>)
;   (define v <e2>)
;   <e3>)

; Upon evlaluation of the `lambda`, a new enviroment E1
; will be created. The internal `define`s will be evaluated,
; binding `u` and `v` in E1 to <e1> and <e2>.
; <e3> is then evaluated in E1.


; b. Transformed version

; (lambda <vars>
;   (let ((u '*unassigned*)
;         (v '*unassigned*))
;     (set! u <e1>)
;     (set! v <e2>)
;     <e3>))

; Upon evaluation, this version will be syntatically
; transformed to:

; (lambda <vars>
;   ((lambda (u v)
;     (set! u <e1>)
;     (set! v <e2>)
;      <e3>
;    )('*unassigned* '*unassigned*)))

; The evaluation of the outer `lambda` will create a new
; enviroment E1, in which <vars> will be bound to the arguments.
; The inner lambda will execute, creating yet another enviroment,
; E2, in which `u` and `v` will be bound to '*unassigned*. <e3>
; will be evaluated in E2.

; In both cases, `u` and `v` will be bound in the same fame <e3>
; is evaluated, so there won't be any difference from the point
; of view of the programmer.

; In order to implement a "simultaneous" scope rule for internal
; definitions so that no new environment is created, we need to get
; rid of the internal `lambda` (which comes from the `let` syntatic
; sugar). One way to do it would be to simply move all definitions
; to the top - a technique used in javascript and known as hoisting.
