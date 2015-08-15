; The exercise asks us to define `one` and `two` directly, without using
; the procedures `zero` and `add-1`.

; Given procedures (Church numerals). I substituted `f` -> `g` and `x` -> `y`
; to make the substitution method analysis easier
(define zero (lambda (g) (lambda (y) y)))
; `zero` returns a procedure that receives `g` as argument
; and returns another procedure that always returns its argument

(define (add-1 n)
   (lambda (f) (lambda (x) (f ((n f) x)))))
; `add-1` is a procedure that receives `n` and returns a procedure that
; receives `f` as agument, which returns a procedure that receives `x`
; as argument and returns the call to `f` with argument `((n f) x)`.

; Let's analyze a the following call:
(add-1 zero)
(add-1 (lambda (g) (lambda (y) y)))

 (lambda (f) (lambda (x) (f ((n f) x))))
 (lambda (f) (lambda (x) (f ((lambda (y) y) x))))
 (lambda (f) (lambda (x) (f x)))

 ; This means we can write `one` only  in terms of `f` and `x`, which I'm substituting
 ; for `h` and `z`, respectively:

 (define one
   (lambda (h) (lambda (z) (h z))))

; Let's analyze the following call:
(add-1 one)
(add-1 (lambda (h) (lambda (z) (h z))))
 (lambda (f) (lambda (x) (f (((lambda (h) (lambda (z) (h z))) f) x))))
 (lambda (f) (lambda (x) (f (((lambda (z) (f z)) x)))))
 (lambda (f) (lambda (x) (f (f x))))

; Again, we can write `(add-1 one)`, only in terms of `f` and `x`, which means
; we can define the procedure `two` as:

(define two
   (lambda (f) (lambda (x) (f (f x)))))

; Now we can kind of see a trend...
(define three
   (lambda (f) (lambda (x) (f (f (f x))))))

(define four
   (lambda (f) (lambda (x) (f (f (f (f x)))))))

; Can we generalize successive calls to `add-1` with a `+` procedure?

; Signature: (define (+ a b) ???)
; `a` is a procedure that receives a procedure and applies it `a` times on a given argument.
; The same works for b. Ideally, we want `(+ a b)` to be a procedure that receives a procedure
; as argument and applies it `a+b` times on a given argument.

; We want something like:
;(define (+ a b)
;  (lambda (f) (lambda (x) (f (f (f (f ... (f x)))))))) ; a + b times

; One way of achieving this is to simply return a procedure that receives a prodecure
; as argument and returns a procedure that calls both `a` and `b` on its argument, which
; will amount to `a+b` calls.
(define (+ a b)
  (lambda (f)
      ;(a (b f))))
      (lambda (x)
      ((a f) x) ((b f) x))))

; How can we test this? Let's try to come up with a procedure that prints a string
; "a" whenever it is called:
(define (call useless-arg) (display "a"))

((zero call) 1)             ; => prints nothing
((one call) 1)              ; => prints "a"
((two call) 1)              ; => prints "aa"
((three call) 1)            ; => prints "aaa"
(((+ two three) call) 1)    ; => prints "aaaaa" (5 times)
(((+ three four) call) 1)   ; => prints "aaaaaaa" (7 times)


; The exercise doesn't ask for this, but if we go a little further, we can
; also implement multiplication between Church numerals.

; We want something like:
;(define (* a b)
;  (lambda (f) (lambda (x) (f (f (f (f ... (f x)))))))) ; a * b times

; We could call `b` with a given procedure (which will return a procedure
; that will be applied `b` times on its input), and call `a` on that procedure,
; which will amount to `a*b` calls:
(define (* a b)
  (lambda (f) (a (b f))))

; Trying this out:
(((* one zero) call) 1)      ; => prints nothing
(((* one one) call) 1)       ; => prints "a"
(((* two three) call) 1)     ; => prints "aaaaaa" (6 times)
(((* three three) call) 1)   ; => prints "aaaaaaaaa" (9 times)
(((* three four) call) 1)    ; => prints "aaaaaaaaaaaa" (12 times)
(((* four four) call) 1)     ; => prints "aaaaaaaaaaaaaaaa" (16 times)

; Sweet! :)
