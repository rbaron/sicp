; `last-pair` rule

; Load this file and drop yourself in the REPL with:
; $ cat 4.62.scm - | mit-scheme

(load "book-code/ch4-query")

(initialize-data-base microshaft-data-base)
(query-driver-loop)

(assert!
  (rule (last-pair (?last . ()) (?last))))

(assert!
  (rule (last-pair (?head . ?tail) (?last))
    (last-pair ?tail (?last))))

; Testing

(last-pair (1 2 3 4) (?last))
; => (last-pair (1 2 3 4) (4))

(last-pair (4) (?last))
; => (last-pair (4) (4))

(last-pair (3) ?x)
; => (last-pair (3) (3))

(last-pair (1 2 3) ?x),
; => (last-pair (1 2 3) (3))

(last-pair (2 ?x) (3))
; => (last-pair (1 2 3) (3))

; Do our rules work on cases like (last-pair ?x (3))?

; By wild-carding the first argument:
; - The first rule won't be triggered, since the first element of
;   ?x is not  (3).
; - The second rule will be triggered, since ?x will expand to
;   (?head . ?tail). ?tail itself is a wild-card, and will recurse
;   in the exact same way, triggering the second rule again.

; TL;DR: program will hang. Uncomment the following line to test it:

;(last-pair ?x (3))
