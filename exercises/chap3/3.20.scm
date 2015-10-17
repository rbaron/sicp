; Mutators as assignments

; Given procedural pair implementation:
(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined operation -- CONS" m))))
  dispatch)

(define (car z) (z 'car))

(define (cdr z) (z 'cdr))

(define (set-car! z new-value)
  ((z 'set-car!) new-value)
  z)

(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)


; Let's analyze the enviroment after calls to:

(define x (cons 1 2))
; => This will look up the `cons` implementation in the global
; enviroment and create a new enviroment E1 for it to be called.
; After the call, `x` will be bound to a specific `dispatch` procedure,
; having as enclosing enviroment E1.


;               +---------------------------+                                  
;               |                           |
; Global env -> |  x: +-----------------+   |
;               |  cons: +-+            |   | <---------------+
;               |          |            |   |                 |
;               +-----------------+---------+                 |
;                          |      ^     |                     |
;                          v      |     |                     |
;                       +-----+   |     |                     |
;                   +----| | |----+     |          +----------+------+
;                   |   +--+--+         |          |  x: 1           |
;                   |                   |          |  y: 2           |
;                   v                   |          |  set-x: ...     |
;             parameters: x y           v      +-> |  set-y: ...     | E1
;             body: (cons body)      +-----+   |   |  dispatch: ...  |
;                                +----| | |----+   |                 |
;                                |   +--+--+       +-----------------+
;                                |
;                                v
;                         parameters: m
;                         body: (dispatch body)

(define z (cons x x))

;                   +---------------------------+
;                   |                           |
;     Global env -> |  z:+--+         x: +----+ | <--------------+
;                   |       |                 | |                |
;            +--------------+                 | |         +------+----------+
;            |      |---------------------------+         |                 |
;            +-------+      ^                 |           |  x: 1           |
;                    v      |                 v        +->+  y: 2           | E1
;                 +-----+   |               |---+--+   |  |  set-x: ...     |
;             +----| | ||   |            +----| | |----+  |  set-y: ...     |
;             |   +--+--+   |            |  |---+--+      |  dispatch: ...  |
;             v        |    |            |                |                 |
; parameters: m        |    |            v                +-----------------+
; body: (dispatch body)|    |       parameters: m
;   (shared with x)    |    |       body: (dispatch body)
;                      v    |              (shared with z)
;                 +----+----+-------+
;                 |                 |
;                 |  x: (pointer x) |
;                 |  y: (pointer x) | E2
;                 |  set-x: ...     |
;                 |  set-y: ...     |
;                 |  dispatch: ...  |
;                 |                 |
;                 +-----------------+


(set-car! (cdr z) 17)

; This will cause `z` to be called with `'cdr` as argument. To evaluate this call,
; a new enviroment E3 will be created, the parameter `m` will be bound to `'cdr`
; and `set-x!` will be called, creating yet another enviroment E4 where the call to
; `(set! x 17)` will happen. This will modify the `x` defined at the global enviroment:

;                   +---------------------------+
;                   |                           |
;     Global env -> |  z:+--+         x: +----+ | <--------------+
;                   |       |                 | |                |
;            +--------------+                 | |         +------+----------+
;            |      |---------------------------+         |                 |
;            +-------+      ^                 |           |  x: 17          |
;                    v      |                 v        +->+  y: 2           | E1
;                 +-----+   |               |---+--+   |  |  set-x: ...     |
;             +----| | ||   |            +----| | |----+  |  set-y: ...     |
;             |   +--+--+   |            |  |---+--+      |  dispatch: ...  |
;             v        |    |            |                |                 |
; parameters: m        |    |            v                +-----------------+
; body: (dispatch body)|    |       parameters: m
;   (shared with x)    |    |       body: (dispatch body)
;                      v    |              (shared with z)
;                 +----+----+-------+
;                 |                 |
;                 |  x: (pointer x) |
;                 |  y: (pointer x) | E2
;                 |  set-x: ...     |
;                 |  set-y: ...     |
;                 |  dispatch: ...  |
;                 |                 |
;                 +-----------------+

(car x)
; => 17
