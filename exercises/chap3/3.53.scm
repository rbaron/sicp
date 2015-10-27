; Analyzing an implicit stream definition

(define s (cons-stream 1 (add-streams s s)))

; Let's analyze the first few elements of this stream.

; t = 0
; First element is 1
; Second element is a promise of a stream resulting of adding
; two `s`s streams,

; t = 1
; First element will be the sum of two `s`s, so 1 + 1 = 2
; The second element will be a promise of adding two times the
; second element of s, which is itself a sum of two times the
; first element of `s`.

; t = 2
; First element will be 4
; Second element will be a promise to evaluate the the sum of
; two sums of two first elements of `s`.


; This is better shown by the recursion tree:

;          1           =   1
;       /     \
;     1    +   1       =   2
;   /   \     /  \
;  (1 + 1) + (1 + 1)   =   4
;       .......
;                      =   8
;       .......
;                      =   16
;       .......


; Trying it out

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (stream-take n stream)
  (if (= n 0)
    '()
    (cons (stream-car stream)
          (stream-take (- n 1) (stream-cdr stream)))))

(stream-take 10 s)
; => (1 2 4 8 16 32 64 128 256 512)
