; Analyzing `set-to-wow!` calls on different structures.

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

; In addition, let `z1` and `z2` be:
(define x (list 'a 'b))
(define z1 (cons x x))

(define z2 (cons (list 'a 'b) (list 'a 'b)))

; At this point, we have:

;       +---+
; z1 -> |.|.|
;       ++-++
;        | |
;        v v
;       ++-++    +---+
;  x -> |a|.+--->+b|/|
;       +---+    +---+


;        +---+
;  z2 -> |.|.|
;        +---+
;         | |
;         | |  +---+     +---+
;         | +> |a|.+---> |b|/|
;         |    +---+     +---+
;         |                      
;         |    +---+     +---+
;         +--> |a|.+---> |b|/|
;              +---+     +---+


; The results of the following calls are:

(set-to-wow! z1)

;       +---+
; z1 -> |.|.|
;       ++-++
;        | |
;        v v
;       ++-++    +---+
;  x -> |.|.+--->+b|/|
;       ++--+    +---+
;        |
;        v
;       'wow

z1
; => ((wow b) wow b)


(set-to-wow! z2)

;        +---+
;  z2 -> |.|.|
;        +---+
;         | |
;         | |  +---+     +---+
;         | +> |a|.+---> |b|/|
;         |    +---+     +---+
;         |                      
;         |    +---+     +---+
;         +--> |.|.+---> |b|/|
;              ++--+     +---+
;               |
;               v
;              'wow

z2
; => ((wow b) a b)
