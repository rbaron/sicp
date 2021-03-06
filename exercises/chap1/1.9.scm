; Procedure 1
; ===========

(define (+ a b)
  (if (= a 0)
    b
    (inc (+ (dec a) b))))

; > (+ 4 5)
; > (inc (+ (dec 4) 5))
; > (inc (+ 3 5))
; > (inc (inc (+ (dec 3) 5)))
; > (inc (inc (+ 2 5)))
; > (inc (inc (inc (+ dec(2) 5))))
; > (inc (inc (inc (+ 1 5))))
; > (inc (inc (inc (inc (+ (dec 1) 5)))))
; > (inc (inc (inc (inc (+ 0 5)))))
; > (inc (inc (inc (inc 5))))
; > (inc (inc (inc 6)))
; > (inc (inc 7))
; > (inc 8)
; > 9

; Procedure 2
; ===========

(define (+ a b)
  (if (= a 0)
    b
    (+ (dec a) (inc b))))

; > (+ 4 5)
; > (+ 3 6)
; > (+ 2 7)
; > (+ 1 8)
; > (+ 0 9)
; > 9
;
;
; Even though both procedures are recursive, it's clear, using the substitution method,
; that the first procedure also generates a recursive process. The process has to keep
; tabs of the "state" of the procedure before calling the procedure again. The process
; should remember it still needs to increment the returned value of the subprocedure by
; calling (inc 'result-of-recursive-call').
;
; The second procedure, on the other hand, doesn't need to keep extra track of its recursive
; calls. All the state needed for it is passed via the `a` and `b` variables to the sub-
; procedures. Thus, even thought the procedure itself is recursive, it can leverage from
; tail recursive optimization to use constant space, resulting in an iterative process.
