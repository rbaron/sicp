; Mutators for pairs - evaluating calls to `append!`

; Let's define an `append!` procedure:
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

; Helper procedure to find the last pair of a list
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

; Let's compare it with our standard `append` procedure:
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))


; Instead of creating new pairs with `cons`, `append!` simply
; iterates over a list until it finds the last pair and mutate
; the `cdr` pointer so it points to `y`.

; Let's analyze some calls:

(define x (list 'a 'b))

(define y (list 'c 'd))

; At this point we have the following structure:
;      +---+   +---+
; x -> |a|-|-->+b|/|
;      +---+   +---+
;
;      +---+   +---+
; y -> |c|-|-->+d|/|
;      +---+   +---+

(define z (append x y))

; Defining `z` doesn't modify `x`:

;      +---+   +---+
; x -> |a|---->+b|/|
;      +---+   +---+
;
;      +---+   +---+
; y -> |c|---->+d|/|
;  +-> +---+   +---+
;  |
;  +------------------+
;                     |
;      +---+   +---+  |
; z -> |a|---->+b|----+
;      +---+   +---+

z
; => (a b c d)

(cdr x)
; => (b)

(define w (append! x y))
; Defining `w` had `x` modified as a side effect, since we iterated
; over `x` and modified its last pair's `cdr` pointer. We now have:

; w -> +---+   +---+
; x -> |a|---->+b|----+
;      +---+   +---+  |
;                     |
;  +------------------+
;  |
;  +-> +---+   +---+
; y|-> |c|---->+d|/|
;  +-> +---+   +---+
;  |
;  +------------------+
;                     |
;      +---+   +---+  |
; z -> |a|---->+b|----+
;      +---+   +---+

w
; => (a b c d)

(cdr x)
; => (b c d)
