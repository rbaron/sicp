; Biblical family tree

; Load this file and drop yourself in the REPL with:
; $ cat 4.63.scm - | mit-scheme

(load "book-code/ch4-query")

(initialize-data-base '())
(query-driver-loop)

; Let's include the mentioned relations in the database

(assert! (son Adam Cain))
(assert! (son Cain Enoch))
(assert! (son Enoch Irad))
(assert! (son Irad Mehujael))
(assert! (son Mehujael Methushael))
(assert! (son Methushael Lamech))
(assert! (wife Lamech Ada))
(assert! (son Ada Jabal))
(assert! (son Ada Jubal))

(assert! (rule (same ?x ?x)))

; Let's define our rules

(assert!
  (rule (grandparent ?G ?S)
    (or (and (son ?G ?P) (son ?P ?S))
        (and (wife ?G ?wife) (son ?wife ?P) (son ?P ?S))
        (and (son ?G ?P) (wife ?P ?wife) (son ?wife ?S)))))

(assert!
  (rule (family ?wife ?husband ?son)
    (and (son ?wife ?son)
         (wife ?husband ?wife))))

; Testing

(grandparent Cain ?gson)
; => (grandparent cain irad)

(family ?wife Lamech ?son)
; => (family ada lamech jubal)
; => (family ada lamech jabal)

(grandparent Methushael ?gson)
; => (grandparent methushael jubal)
; => (grandparent methushael jabal)
