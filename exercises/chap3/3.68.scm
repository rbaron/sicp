; Analyzint Louis' alternative `pair` procedure

; Louis Reasoner proposes the following version:

(define (pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
               t)
   (pairs (stream-cdr s) (stream-cdr t))))

; Comparing it with the original one:

(define (pairs-orig s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs-orig (stream-cdr s) (stream-cdr t)))))

; We can see that Louis' version lacks the call to
; `cons-stream`, which is a special form that delays
; the execution of the second argument. Without that,
; `interleave` will evaluate its second argument, which
; is a recursive call. That wil produce an infinite loop
; what will cause the program to hang (or reach max recursion
; depth).

; ========================================
;    Begin copied code for testing
; ========================================

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define ones (cons-stream 1 ones))

(define integers
  (cons-stream 1 (add-streams ones integers)))

(define (display-inf-stream s n-terms)
  (define (iter s n-terms)
    (if (> n-terms 0)
      (begin (newline)(display (stream-car s))
             (iter (stream-cdr s) (- n-terms 1)))))
  (newline)
  (iter s n-terms))

; ========================================
;    END OF copied code for testing
; ========================================

(pairs integers integers)
; => ;Aborting!: maximum recursion depth exceeded
