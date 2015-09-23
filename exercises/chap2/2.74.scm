; Insatiable Enterprises, Inc.

; Using data-directed programming to create an
; uniform interface for different implementations.

; a. Implement a universal `get-record` procedure,
;    that works on all divisions.

(define (get-record employee-name personnel-file)
  (apply-generic 'get-record employee-name personnel-file))

; => This will lookup the specific `get-record` implemen-
; tation on the table. For this to work, each division's
; file should be tagged with a type. As a suggestion,
; each division could be assigned a number and tag their
; records with 'division1, 'division2 etc.

; b. Implement a universal `get-salary` procedure.

(define (get-salary employee-record)
  (apply-generic 'get-salary employee-record))

; => Again, for this to work, records should be tagged
; and the actual procedure to fetch the salary should be
; on the our table.

; c. Implementing a `find-employee-record` procedure.

(define (find-employee-record employee-name all-personnel-files)
  (cond ((null? all-personnel-files) #f)
        ((not (eq? (get-record employee-name (car all-personnel-files)) #f))
          ((get-record employee-name (car all-personnel-files))))
        (else
          (find-employee-record employee-name (cdr all-personnel-files)))))

; d. What if Insatiable Enterprises, Inc. acquires a new company?

; => All they'll have to do is get the new company to respect this
; common interface. That is, tag their data and provide us with the
; implementations on our lookup table.
