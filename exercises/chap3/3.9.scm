; Enviroment model of evaluation - analyzing the recursive and iterative
; versions of `factorial`


; Let's start with the recursive version. Here it is:

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

; The `factorial` procedure is not bind to the "factorial" name in
; the global frame. That is, the `factorial` procedure is actually
; a pair, containing the body of the procedure and the reference to
; the frame in which it is bound - in this case, the global frame.

; Let's trace it calls and pay attention to the frames we're creating
; along the way

(factorial 6)

; This call will look up the "factorial" binding in the global frame
; and create a new enviroment E1, in which its formal parameters will
; be bound. In this case, we'll have:

; E1 { n: 6} -> Global frame

; Now, while evaluating the body of `factorial`, we will find a recursive
; call to:

(factorial 5)

; Again, a new enviroment E2 will be created, having as enclosing enviro-
; ment the global one:

; E2 { n: 5 } -> Global frame

; Same goes for all the recursive calls until

(factorial 1)

; Which creates the last enviroment needed, E6, that contains:

; E6 { n: 1 } -> Global frame

; Now we're gonna start to "bottom up" the recursion, by returning 1.
; Once the last level of recursion returned, on E6, the upper level of
; recursion will consume it on E5. In its turn, it's gonna multiply it
; by n = 2 and return to the enviroment E4.

; In the end, we will consume the result of `(factorial 5)` in the
; enviroment E6, when we will finally multiply it by n = 6 and return
; it.


; Let's now analyze the iterative version:

(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

; `factorial` and `fact-iter` are now a procedures bound in the global frame.

; GLOBAL FRAME { factorial: "factorial body and pointer to the global frame"
;                fact-iter: "fact-iter body and pointer to the global frame"}

; Calling

(factorial 6)

; Will lookup the "factorial" name in the global frame and create a new
; execution frame, E1, in which "n" will be bound to 6.

; E1 { n: 6 } -> Global enviroment

; Inside the body of `factorial`, another procedure will be looked up and
; executed. "fact-iter" will create yet another enviroment, E2, in which
; it's formal parameters will be evaluated and bound:

; E2 { product: 1
;      counter: 1
;      max-count: 6} -> Global frame

; Recursions on `fact-iter`:

; E3 { product: 1
;      counter: 2
;      max-count: 6} -> Global frame

; E4 { product: 2
;      counter: 3
;      max-count: 6} -> Global frame

; E5 { product: 6
;      counter: 4
;      max-count: 6} -> Global frame

; E6 { product: 24
;      counter: 5
;      max-count: 6} -> Global frame

; E7 { product: 120
;      counter: 6
;      max-count: 6} -> Global frame

; E8 { product: 720
;      counter: 7
;      max-count: 6} -> Global frame

; The recursive calls now are going to return (let's forget about tail optimization
; for now). This means the value product = 120 will be forwarded until the first
; recursive call on E2, and finally to E1, in which it will be returned to the
; global frame.
