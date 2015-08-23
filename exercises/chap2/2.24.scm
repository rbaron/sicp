; This exercise asks us to discuss the following construct:

(list 1 (list 2 (list 3 4)))
; Interpreter evaluates to: (1 (2 (3 4)))

; Let's recall how `list` works:

; (list 1 2 3 4)

; is the same as:

; (cons 1 (cons 2 (cons 3 (cons 4 nil))))

; Analogously:

; (list 1 (list 2 (list 3 4)))

; is the same as

; (list 1 (list 2 (list 3 4)))
; (cons 1 (cons (list 2 (list 3 4)) nil))
; (cons 1 (cons (cons 2 (cons (list 3 4) nil)) nil))
; (cons 1 (cons (cons 2 (cons (cons 3 (cons 4 nil)) nil)) nil))

; The box-and-pointer and tree representations are
; hand-drawn on 2.24.jpg.
