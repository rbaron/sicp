; All possible outcomes from two concurrent procedures

; (define x 10)
;
; (parallel-execute (lambda () (set! x (* x x)))
;                   (lambda () (set! x (* x x x))))


; Here are some possible execution orders:

;   t        p1         p2
;
;   1      get x 10
;   2      get x 10
;   3      set x 1e2
;   4                 get x 1e2
;   5                 get x 1e2
;   6                 get x 1e2
;   7                 set x 1e6
;
; => x = 1e6

;   t        p1         p2
;
;   1                 get x 10
;   2                 get x 10
;   3                 get x 10
;   4                 set x 1e3
;   5      get x 1e3
;   6      get x 1e3
;   7      set x 1e6
;
; => x = 1e6


;   t        p1         p2
;
;   1      get x 10
;   2      get x 10
;   3                 get x 10
;   4      set x 1e2
;   5                 get x 1e2
;   6                 get x 1e2
;   7                 set x 1e5
;
; => x = 1e5


;   t        p1         p2
;
;   1      get x 10
;   2      get x 10
;   3                 get x 10
;   4                 get x 10
;   5      set x 1e2
;   6                 get x 1e2
;   7                 set x 1e4
;
; => x = 1e4


;   t        p1         p2
;
;   1      get x 10
;   2      get x 10
;   3                 get x 10
;   4                 get x 10
;   5                 get x 10
;   6      set x 1e2
;   7                 set x 1e3
;
; => x = 1e3


;   t        p1         p2
;
;   1      get x 10
;   2      get x 10
;   3                 get x 10
;   4                 get x 10
;   5                 get x 10
;   6                 set x 1e3
;   7      set x 1e2
;
; => x = 1e2


;   t        p1         p2
;
;   1      get x 10
;   2                 get x 10
;   3                 get x 10
;   4                 get x 10
;   5                 set x 1e3
;   6      get x 1e3
;   7      set x 1e4
;
; => x = 1e4


; After the serialization, only the first and second scenario
; are possible. Both yield 1e6.
