; Representing Lisp structures with box-and-pointer and memory vector

; The drawing is in 5.20.png.

; There is, however, a question. When representing the structure:

(list 1 2 3)

; It will be expanded to

(cons 1 (cons 2 (cons 3 '())))

; So, I'd imagine the first allocated pair would be

(cons 3 '())

; Which would occupy the first free position (say, 1). Then

(cons 2 *pointer-to-1*)

; Would be allocated in position 2. Finally,

(cons 1 *pointer-to-2*)

; Would be allocated in position 3. The memory layout,
; in the end, would be "backwards".

; I used this rationale in my solution, even though every other
; solution I found online didn't took this into account.
