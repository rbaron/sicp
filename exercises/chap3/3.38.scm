; Evaluating nondeterministic concurrent computations

; Peter, Paul and Mary have a shared bank account initially
; with $100. The following concurrent operations are
; performed:

; Peter:  (set! balance (+ balance 10))
; Paul:   (set! balance (- balance 20))
; Mary:   (set! balance (- balance (/ balance 2)))

; a. What are the possible values of `balance` at the end
; of the transactions for every possible order of
; execution?

; We have 3! permutations:

; 1 2 3 => b = (100 + 10 - 20)/2 = 65
; 1 3 2 => b = (100 + 10)/2 - 20 = 35
; 2 1 3 => b = (100 - 20 + 10)/2 = 65
; 2 3 1 => b = (100 - 20)/2 - 10 = 30
; 3 1 2 => b = (100)/2 + 10 - 20 = 40
; 3 2 1 => b = (100)/2 - 20 + 10 = 40

; b. What if we allow concurrent execution?

; Let's first list all actions required by each operation:

; Peter:
; (set! balance (+ balance 10))
; -> get balance -> increment balance by 10

; Paul:
; (set! balance (- balance 20))
; -> get balance -> decrement balance by 20

; Mary:
; (set! balance (- balance (/ balance 2)))
; -> get balance -> halve balance

; We now have 6 operations, since we broke  each sequential unit
; into two consecutive parts. Figuring out how many combinations
; there are is not very straight forward, but it can be done using
; a graph to analyze possible combinations.

; A few possible outcomes are:

; 1.

;   t      Peter +10      Paul -20        Mary /2
;
;   1      get 100
;   2                     get 100
;   3      set 110
;   4                                    get 110
;   5                                    set 55
;   6                     set 35
;
; Result => 35


; 2.

;   t      Peter +10      Paul -20        Mary /2
;
;   1                     get 100
;   2                                    get 100
;   3      get 100
;   4                                    set 50
;   5                     set 80
;   6      set 110
;
; Result => 110


; 3.

;   t      Peter +10      Paul -20        Mary /2
;
;   1                     get 100
;   2                                    get 100
;   3                                    set 50
;   4                     set 80
;   5      get 80
;   6      set 90
;
; Result => 90
