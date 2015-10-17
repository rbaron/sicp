; `mystery` procedure evaluation


(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))


; Let's analyze the internal `loop` procedure. The recursion case
; calls `loop` again with the old `cdr` as first argument and `x`,
; mutated with `y` to be its `cdr` as the second argument. Let's
; simulate a few calls to loop to get a better feeling what's
; going on.

;                 Call table for `loop`
;
;     x                y        temp        new-x
;  (a b c)            ()        (b c)       (a)
;  (b c)              (a)       (c)         (b a)
;  (c)                (b a)     ()          (c b a)

; => (c b a)

; The `mystery` procedure reverses a list!

; Let's now analyze the specific call suggested by the exercise:

(define v (list 'a 'b 'c 'd))

(define w (mystery v))

; We can see in the definition of `loop` that the only mutator
; present is the `set-cdr!`, which will modify `v` in the first
; iteration. `v` will become:

;      +---+
; v -> |a|/|
;      +---+

; In the end of the process, `w` points to the reversed list:

;      +---+   +---+   +---+   +---+
; w -> +d|-|--->c|-|--->b|-|--->a|/|
;      +---+   +---+   +---+   +---+

; Trying it out:

v
; => (a)

w
; => (d c b a)
