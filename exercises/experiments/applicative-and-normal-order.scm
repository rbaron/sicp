; Scheme works with applicative order of evaluation. Example:

(define test (lambda (a) (display "\nEval'd body")))

(test (begin
        (display "\nEval'd arg")
         1))

; Prints
; => Eval'd arg
; => Eval'd body
