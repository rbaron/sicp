; Analyzing the stream version of the Fibonacci sequence

; The book introduced the following procedure:

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))

; How many additions are performed when computing the n-th
; term?

; I had a hard time mentally keeping tabs on how many additions
; are performed with memoization in this case. I tried drawing
; what the recursion tree alongside with a memoization table
; in figure 3.57.a.jpg. It's clear that the number of additions
; performed is linear in the index of the Fibonacci sequence.
; To calculate the n-th element of the sequence (0 indexed),
; n-1 additions are performed.

; Should the stream not be memoized, we would expand each node
; to produce a full-blown tree representing the sequence. We
; can see this in figure 3.58.b.jpg.
; The number of additions in each level is:

; 0 -> 0
; 1 -> 0
; 2 -> 1
; 3 -> 2
; 4 -> 4
; 5 -> 7

; Which yields the recursion:

; N(l) = N(l-1) + N(l-2) + 1

; Which has a exponential solution.
