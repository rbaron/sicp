; Multiple wheels

; Load this file and drop yourself in the REPL with:
; $ cat 4.63.scm - | mit-scheme

(load "book-code/ch4-query")

(initialize-data-base microshaft-data-base)
(query-driver-loop)

; Using the rule
; (rule (wheel ?person)
;       (and (supervisor ?middle-manager ?person)
;            (supervisor ?x ?middle-manager)))

; the query
; (wheel ?who)

; prints
; (wheel (Warbucks Oliver))
; (wheel (Bitdiddle Ben))
; (wheel (Warbucks Oliver))
; (wheel (Warbucks Oliver))
; (wheel (Warbucks Oliver))

; Why does "Warbucks Oliver" appear four times?

; => Let's try to follow the unification process.
; - Initially, a stream consisting of an empty frame is fed to the
;   assertion. The unification process binds ?person to ?who.
; - The first assertion in `and` scans the database for matches
;   and generates a stream, in which each frame will have
;   ?middle-manager bound to someone and ?person bound to their manager.
;   This is the initial stream:

(supervisor ?middle-manager ?person)
; => (supervisor (aull dewitt) (warbucks oliver))
; => (supervisor (cratchet robert) (scrooge eben))
; => (supervisor (scrooge eben) (warbucks oliver))
; => (supervisor (bitdiddle ben) (warbucks oliver))
; => (supervisor (reasoner louis) (hacker alyssa p))
; => (supervisor (tweakit lem e) (bitdiddle ben))
; => (supervisor (fect cy d) (bitdiddle ben))
; => (supervisor (hacker alyssa p) (bitdiddle ben))

; - The second assertion will expand _each_ element of the previous
;   stream by generating a stream in which ?x will be bound to anyone
;   who is supervised by the previously bound ?middle-manager. Where
;   the query is satisfied, ?person will be bound to the original ?who,
;   which will cause it to be shown repeated times.
