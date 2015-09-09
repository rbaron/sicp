; Quote-quote evaluation: why does `(car ''abracadabra)` evaluates
; to `quote`?

; Let's try a simple example:
> 'a
;Value: a

> ''a
;Value: (quote a)

> (quote a)
;Value: a

; 1. It seems like `'` is syntatic sugar for the `quote` procedure.

; 2. Calling `quote` on a procedure makes the procedure name and
; all its arguments to be evaluated as symbols, as if it were a
; list of symbols:

> (quote (car a))
;Value: (car a)

; 3. Calling `quote` on the `(quote a)`, makes the interpreter interpret
; it as a list of symbols, so, as expected:

> (quote (quote a))
;Value: (quote a)

; Which is the same as `''a`:

> ''a
;Value: (quote a)
