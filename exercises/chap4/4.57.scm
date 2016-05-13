; Load this file and drop yourself in the REPL with:
; $ cat 4.57.scm - | mit-scheme

(load "book-code/ch4-query")

(initialize-data-base microshaft-data-base)
(query-driver-loop)

; Rule: person 1 can replace person 2 if either person 1 does
; the same job as person 2 or someone who does person 1's job
; can also do person 2's job, and if person 1 and person 2 are
; not the same person

(assert! (rule (can-replace? ?p1 ?p2)
  (and
    (job ?p1 ?job1)
    (job ?p2 ?job2)
    (or (same ?job1 ?job2)
        (can-do-job ?job1 ?job2))
    (not (same ?p1 ?p2)))))

; a. all people who can replace Cy D. Fect;
(can-replace? ?person (Fect Cy D))

; => (can-replace? (bitdiddle ben) (fect cy d))
; => (can-replace? (hacker alyssa p) (fect cy d))

; b. all people who can replace someone who is being paid more
;    than they are, together with the two salaries.
(and (can-replace? ?p1 ?p2)
     (salary ?p1 ?p1-sal)
     (salary ?p2 ?p2-sal)
     (lisp-value > ?p2-sal ?p1-sal))

; => (and (can-replace? (aull dewitt) (warbucks oliver)) (salary (aull dewitt) 25000) (salary (warbucks oliver) 150000) (lisp-value > 150000 25000))
; => (and (can-replace? (fect cy d) (hacker alyssa p)) (salary (fect cy d) 35000) (salary (hacker alyssa p) 40000) (lisp-value > 40000 35000))

