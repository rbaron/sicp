; Making cycles using mutators

; Given procedures
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

; Let's draw a diagram to analyze the call to:
(define z (make-cycle (list 'a 'b 'c)))

; Let's break it down. Let's define:
(define x (list 'a 'b 'c))

; We have:

;     +---+   +---+   +---+
; x ->+a|----->b|----->c|/|
;     +---+   +---+   +---+

; Calling `make-cycle` on `x` will cause the last
; pair's `cdr` pointer to point to `x` itself:

;     +---+   +---+   +---+
; x ->+a|----->b|----->c|----+
; +-->+---+   +---+   +---+  |
; |                          |
; +--------------------------+

; Calling `last-pair` on `x` now would create an infinite
; loop, because the predicate `null? (cdr x)` will never be
; true.
