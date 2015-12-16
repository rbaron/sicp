; Side effects in sequence of expressions

; a. Ben suggests that the `for-each` implementation and
; use works correctly:

(define (for-each proc items)
  (if (null? items)
      'done
      (begin (proc (car items))
             (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

; Ben is correct. Since `display` is a primitive procedure,
; the arguments for it will be forced. So the side effects
; will take place.

; b. Cy suggests the following procedures to be evaluated by
; our lazy interpreter:

(define (p1 x)
  (set! x (cons x '(2)))
  x)

(define (p2 x)
  (define (p e)
    e ; here `e` will be a thunk - it won't be forced!
    x)
  (p (set! x (cons x '(2)))))

;;; L-Eval input:
(p1 1)

;;; L-Eval value:
(1 2)

;;; L-Eval input:
(p2 1)

;;; L-Eval value:
1

; With Cy's version of `eval-sequence`, the thunk would be forced inside
; `p`. So `(p2 1)` would return `(1 2)`.

; c. In part a., it wouldn't make a different using the original of Cy's
; version of `eval-sequence`, since the arguments are used in a primitive
; procedure (`display`), and so they are forced anyway.

; d. In my opinion, both Ben and Cy have nice approachs:
; - Ben's (original) version seems more efficient, since an unused expression
; will not be evaluated;
; - Cy's version is somewhat "safer", since every expression will be evaluated.
