; Adding meetings to database

; Load this file and drop yourself in the REPL with:
; $ cat 4.59.scm - | mit-scheme

(load "book-code/ch4-query")

(initialize-data-base microshaft-data-base)
(query-driver-loop)

; Bed Bitwiddle added the following entries to the database:

(assert! (meeting accounting (Monday 9am)))
(assert! (meeting administration (Monday 10am)))
(assert! (meeting computer (Wednesday 3pm)))
(assert! (meeting administration (Friday 1pm)))
(assert! (meeting whole-company (Wednesday 4pm)))

; a. On Friday morning, Ben wants to query the data base for all
; the meetings that occur that day. What query should he use?

(meeting ?dep (Friday ?time))
; => (meeting administration (friday 1pm))

; b. Alyssa P. Hacker is unimpressed. She thinks it would be much
; more useful to be able to ask for her meetings by specifying her name.
; So she designs a rule that says that a person's meetings include all
; whole-company meetings plus all meetings of that person's division.
; Fill in the body of Alyssa's rule.

; (rule (meeting-time ?person ?day-and-time)
;       <rule-body>)

(assert!
	(rule (meeting-time ?person ?day-and-time)
		(or (meeting whole-company ?day-and-time)
				(and (job ?person (?dept . ?titles))
						 (meeting ?dept ?day-and-time)))))

(meeting-time ?person ?day-and-time)

; => (meeting-time ?person-1 (wednesday 4pm))
; => (meeting-time (aull dewitt) (friday 1pm))
; => (meeting-time (cratchet robert) (monday 9am))
; => (meeting-time (aull dewitt) (monday 10am))
; => (meeting-time (scrooge eben) (monday 9am))
; => (meeting-time (warbucks oliver) (friday 1pm))
; => (meeting-time (reasoner louis) (wednesday 3pm))
; => (meeting-time (warbucks oliver) (monday 10am))
; => (meeting-time (tweakit lem e) (wednesday 3pm))
; => (meeting-time (fect cy d) (wednesday 3pm))
; => (meeting-time (hacker alyssa p) (wednesday 3pm))
; => (meeting-time (bitdiddle ben) (wednesday 3pm))

; c. Alyssa arrives at work on Wednesday morning and wonders what meetings
; she has to attend that day. Having defined the above rule, what query
; should she make to find this out?

(meeting-time (Hacker Alyssa P) (Wednesday ?time))

; => (meeting-time (hacker alyssa p) (wednesday 4pm))
; => (meeting-time (hacker alyssa p) (wednesday 3pm))
