; Devising a loop-avoidance system

; Let's investigate a scenario in which a loop arises. We saw one in
; exercise 4.64:

; (rule (outranked-by ?staff-person ?boss)
;       (or (supervisor ?staff-person ?boss)
;           (and (outranked-by ?middle-manager ?boss)
;                (supervisor ?staff-person ?middle-manager))))

; With the call to:
; (outranked-by (Bitdiddle Ben) ?who)

; When outranked-by is evaluated the first time, a new frame f1 will be
; created (by extending the empty frame), with the following binds:

; ?staff-person -> (Bitwiddle Ben)
; ?boss -> ?who

; Upon evaluation, the body of te assertion will cause the same assertion
; to be evaluated, creating a new frame f2 with the bindings:
; ?staff-person -> ?middle-manager
; ?boss -> ?boss -> ?who

; Again, the assertion will be evaluated recursively in frame f3, with:
; ?staff-person -> ?middle-manager
; ?boss -> ?boss -> ?who

; The problem arises from the fact that assertions with the same variables
; are called over and over, recursively. One way to solve this problem is
; to pass along the stream chain a list of frames, which contain all the previously
; evaluated instantiations.
; Whenever a new frame would be generated, we check if an equivalent frame is
; in the list. If it is, skip. If not, add it to the stream.
