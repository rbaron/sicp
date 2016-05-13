; Examples of compound queries

; a. the names of all people who are supervised by Ben Bitdiddle, together with their addresses;

; => (and (supervisor ?person (Ben Bitdiddle))
;         (address ?person ?address))

; b. all people whose salary is less than Ben Bitdiddle's, together with their salary and Ben Bitdiddle's salary;

; => (and (salary (Ben Bitwiddle) ?ben-salary)
;         (salary ?name ?salary)
;         (lisp-value < ?salary ?ben-salary))

; c. all people who are supervised by someone who is not in the computer division, together with the supervisor's name and job.

; (and (not (job ?supervisor (computer . ?job-type)))
;      (job ?supervisor ?supervisor-job)
;      (supervisor ?name ?supervisor))
