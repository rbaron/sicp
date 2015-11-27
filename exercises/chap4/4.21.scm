; Specifying recursive procedures withouth `letrec`

; a. The book suggests the following procedure works
; for calculating factorials:

((lambda (n)
   ((lambda (fact)
      (fact fact n))
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1)))))))
 10)

; Let's analyze this using the enrionment model of computation.

; 1. Calling the expression results in a call of the form:
; (<lambda> 10)

; The argument 10 evaluates to itself.

; 2. The topmost `lambda` is executed, creating an environment
; E1 in which `n` is bound to 10.

; 3. Inside the topmost lambda, we have an expression like:
; (<lambda2> <lambda3>). <lambda3> is the argument of <lambda2>,
; so <lambda3> is evaluated to a procedure whose closing environ-
; ment is E1.

; 4. (<lambda2> <lambda3>) is evaluated. A new environment E2 is
; created, in which the procedure "lambda3" is bound to `fact`.

; 5. `(fact fact n)` is evaluated. This creates an environment E3,
; whose closing environment is E2. In E2, `fact` will be bound to `ft`
; and `k` will be bound to 10.

; 6. The call from 5. will recurse. In evaluating the recursive call,
; environment E4 will be created to evaluate `(ft ft 9)`.

; 7. The recursion will go on until `k` is 1. At which point the recursive
; calls return and the value is computed as `10*9*...*2*1`.


; b. Let's use the same idea to implement the following procedure without
; using neither internal `define`s or `letrec`:

; (define (f x)
;   (define (even? n)
;     (if (= n 0)
;         true
;         (odd? (- n 1))))
;   (define (odd? n)
;     (if (= n 0)
;         false
;         (even? (- n 1))))
;   (even? x))

(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0)
       true
       (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0)
       false
       (ev? ev? od? (- n 1))))))

(f 10)
; => #t

(f 15)
; => #f
