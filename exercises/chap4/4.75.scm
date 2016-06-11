; Implementing unique

; Load this file and drop yourself in the REPL with:
; $ cat 4.75.scm - | mit-scheme

(load "book-code/ch4-query")
(initialize-data-base microshaft-data-base)

; Let's define a procedure for the unique special form:

;(define (negate operands frame-stream)
;  (stream-flatmap
;   (lambda (frame)
;     (if (stream-null? (qeval (negated-query operands)
;                              (singleton-stream frame)))
;         (singleton-stream frame)
;         the-empty-stream))
;   frame-stream))

(define (unique operands frame-stream)
  (stream-flatmap
     (lambda (frame)
       (one-or-empty
         (qeval (car operands) (singleton-stream frame))))
     frame-stream))

(define (one-or-empty stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((stream-null? (stream-cdr stream)) stream)
        (else the-empty-stream)))

(put 'unique 'qeval unique)

; Let's insert our implementation on the operations table
(query-driver-loop)

; Testing

(unique (job ?x (computer wizard)))
; => (unique (job (bitdiddle ben) (computer wizard)))

(unique (job ?x (computer programmer)))
; => Empty

(and (job ?x ?j) (unique (job ?anyone ?j)))
; => (and (job (aull dewitt) (administration secretary)) (unique (job (aull dewitt) (administration secretary))))
; => (and (job (cratchet robert) (accounting scrivener)) (unique (job (cratchet robert) (accounting scrivener))))
; => (and (job (scrooge eben) (accounting chief accountant)) (unique (job (scrooge eben) (accounting chief accountant))))
; => (and (job (warbucks oliver) (administration big wheel)) (unique (job (warbucks oliver) (administration big wheel))))
; => (and (job (reasoner louis) (computer programmer trainee)) (unique (job (reasoner louis) (computer programmer trainee))))
; => (and (job (tweakit lem e) (computer technician)) (unique (job (tweakit lem e) (computer technician))))
; => (and (job (bitdiddle ben) (computer wizard)) (unique (job (bitdiddle ben) (computer wizard))))


; People who supervise precisely one person:

(and (job ?supervisor ?job)
     (unique (supervisor ?supervised ?supervisor)))
; => (and (job (scrooge eben) (accounting chief accountant)) (unique (supervisor (cratchet robert) (scrooge eben))))
; => (and (job (hacker alyssa p) (computer programmer)) (unique (supervisor (reasoner louis) (hacker alyssa p))))
