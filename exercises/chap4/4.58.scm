; Define a rule that says that a person is a ``big shot'' in a division if
; the person works in the division but does not have a supervisor who
; works in the division.

; Load this file and drop yourself in the REPL with:
; $ cat 4.58.scm - | mit-scheme

(load "book-code/ch4-query")

(initialize-data-base microshaft-data-base)
(query-driver-loop)

(assert! (rule (big-shot? ?person ?division)
  (and (job ?person (?division . ?job-types))
       (or (not (supervisor ?person ?supervisor))
           (and (supervisor ?person ?supervisor)
                (not (job ?supervisor (?division . ?supervisor-job-types))))))))

; Testing

(big-shot? ?person ?division)
; => (big-shot? (warbucks oliver) administration)
; => (big-shot? (scrooge eben) accounting)
; => (big-shot? (bitdiddle ben) computer)

