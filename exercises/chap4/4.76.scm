; More efficient coinjoin

; Load this file and drop yourself in the REPL with:
; $ cat 4.76.scm - | mit-scheme

(load "book-code/ch4-query")
(initialize-data-base microshaft-data-base)

; Note: this implementation will work only for two clauses.
; In order to efficiently make more clauses work, I'd approach
; this problem similarly to what merge sorte does: divide
; and conquer.
(define (conjoin2 conjuncts frame-stream)
  (let ((frame-stream1 (qeval (car conjuncts) frame-stream))
        (frame-stream2 (qeval (cadr conjuncts) frame-stream)))
    ;(display "\nClause 1: ")
    ;(display frame-stream1)
    ;(display "\nClause 2: ")
    ;(display frame-stream2)
    (find-compatible-frames frame-stream1 frame-stream2)))

(define (find-compatible-frames stream1 stream2)
  (if (stream-null? stream1)
    the-empty-stream
    (let ((compatibles (all-compatibles (stream-car stream1) stream2)))
      (stream-append compatibles
                    (find-compatible-frames (stream-cdr stream1) stream2)))))

(define (all-compatibles frame frame-stream)
  ;(display "\nAny compatible between: ")(display frame)
  ;(display "\nAnd: ")(display frame-stream)
  (let ((assertions
    (stream-filter
      (lambda (frame) (not (eq? 'failed frame)))
      (stream-map
        (lambda (f) (merge-if-compatible frame f))
        frame-stream))))
      assertions))

(define (merge-if-compatible frame1 frame2)
  ;(display "\nMerge between: ")(display frame1)
  ;(display "\nAnd: ")(display frame2)
  (fold-left
    (lambda (frame binding)
      (if (eq? frame 'failed)
        'failed
        (extend-if-possible (binding-variable binding)
                            (binding-value binding)
                            frame)))
    frame2
    frame1))


(put 'and2 'qeval conjoin2)

(query-driver-loop)

; Testing

(and2 (job ?who ?job)
      (supervisor ?someone ?who))
; => (and2 (job (scrooge eben) (accounting chief accountant)) (supervisor (cratchet robert) (scrooge eben)))
; => (and2 (job (warbucks oliver) (administration big wheel)) (supervisor (aull dewitt) (warbucks oliver)))
; => (and2 (job (warbucks oliver) (administration big wheel)) (supervisor (scrooge eben) (warbucks oliver)))
; => (and2 (job (warbucks oliver) (administration big wheel)) (supervisor (bitdiddle ben) (warbucks oliver)))
; => (and2 (job (hacker alyssa p) (computer programmer)) (supervisor (reasoner louis) (hacker alyssa p)))
; => (and2 (job (bitdiddle ben) (computer wizard)) (supervisor (tweakit lem e) (bitdiddle ben)))
; => (and2 (job (bitdiddle ben) (computer wizard)) (supervisor (fect cy d) (bitdiddle ben)))
; => (and2 (job (bitdiddle ben) (computer wizard)) (supervisor (hacker alyssa p) (bitdiddle ben)))


(and (job ?who ?job)
     (supervisor ?someone ?who))
; => (and (job (scrooge eben) (accounting chief accountant)) (supervisor (cratchet robert) (scrooge eben)))
; => (and (job (warbucks oliver) (administration big wheel)) (supervisor (aull dewitt) (warbucks oliver)))
; => (and (job (hacker alyssa p) (computer programmer)) (supervisor (reasoner louis) (hacker alyssa p)))
; => (and (job (warbucks oliver) (administration big wheel)) (supervisor (scrooge eben) (warbucks oliver)))
; => (and (job (bitdiddle ben) (computer wizard)) (supervisor (tweakit lem e) (bitdiddle ben)))
; => (and (job (warbucks oliver) (administration big wheel)) (supervisor (bitdiddle ben) (warbucks oliver)))
; => (and (job (bitdiddle ben) (computer wizard)) (supervisor (fect cy d) (bitdiddle ben)))
; => (and (job (bitdiddle ben) (computer wizard)) (supervisor (hacker alyssa p) (bitdiddle ben)))
