; Using environments instead variable renaming in rule applications

; Load this file and drop yourself in the REPL with:
; $ cat 4.78.scm - | mit-scheme

; Let's follow the execution of a rule for the following query:

; Query: (lives-near? ?p1 ?someone)
;
; 1) Rule will be transformed and qeval'd in an empty frame
;
; Query: (lives-near? (? p1) (? someone))
; Frame: Empty
;
; 2) Rules will be fetch from the indexed stream. In this case, since the
;    index is 'lives-near?, only one rule will be fetched:
;
; Rule:
;   ((lives-near ?person-1 ?person-2)
;     (and (address ?person-1 (?town . ?rest-1))
;          (address ?person-2 (?town . ?rest-2))
;          (not (same ?person-1 ?person-2))))
;
; 3) Execution of apply-a-rule
;
; 3.1) Rule renaming
;
; Clean Rule:
;   ((lives-near (? 1 person-1) (? 1 person-2))
;     (and (address (? 1 person-1) ((? 1 town) . (? 1 rest-1)))
;          (address (? 1 person-2) ((? 1 town) . (? 1 rest-2)))
;          (not (same (? 1 person-1) (? 1 person-2)))))
;
; 3.2) Unification of rule conclusion and query pattern
;
; Rule conclusion: (lives-near (? 1 person-1) (? 1 person-2))
; Query: (lives-near? (? p1) (? someone))
;
; Unification result (it's a frame):
;   (? p1)      => (? 1 person-1)
;   (? someone) => (? 1 person-2)
;
; 3.3) qeval of rule body using the unification result as a singleton frame
;
; (qeval
;     (and (address (? 1 person-1) ((? 1 town) . (? 1 rest-1)))
;          (address (? 1 person-2) ((? 1 town) . (? 1 rest-2)))
;          (not (same (? 1 person-1) (? 1 person-2))))
;     (((? p1) (? 1 person-1) ((? someone) (? 1 person-2)))))
;
; 4) qeval and
;
; 4.1) First clause
;
; Query: (address (? 1 person-1) ((? 1 town) . (? 1 rest-1)))
; Frame: ((? p1) (? 1 person-1) ((? someone) (? 1 person-2)))
;
; Resulting frame: (((? 1 rest-1) (onion square) 5) ((? 1 town) . slumerville) ((? 1 person-1) aull dewitt) ((? someone) ? 1 person-2) ((? p1) ? 1 person-1))
;
; 4.2) Second clause
;
; Query: (address (? 1 person-2) ((? 1 town) . (? 1 rest-2)))
; Frame: (((? 1 rest-1) (onion square) 5) ((? 1 town) . slumerville) ((? 1 person-1) aull dewitt) ((? someone) ? 1 person-2) ((? p1) ? 1 person-1))
;
; Resulting frame: (((? 1 rest-2) (onion square) 5) ((? 1 person-2) aull dewitt) ((? 1 rest-1) (onion square) 5) ((? 1 town) . slumerville) ((? 1 person-1) aull dewitt) ((? someone) ? 1 person-2) ((? p1) ? 1 person-1))
;
; 4.3) Third clause
;
; Query: (not (same (? 1 person-1) (? 1 person-2)))
; Frame: (((? 1 rest-2) (onion square) 5) ((? 1 person-2) aull dewitt) ((? 1 rest-1) (onion square) 5) ((? 1 town) . slumerville) ((? 1 person-1) aull dewitt) ((? someone) ? 1 person-2) ((? p1) ? 1 person-1))
;
; 4.3.1) Rule evaluation
;
; 4.3.1.1) Rule renaming
;
; Clean Rule: (same? (? 2 x) (? 2 x))
;
; 4.3.1.2) Rule conclustion and query unification
;
; Query: (same (? 1 person-1) (? 1 person-2))
; Rule conclusion: (same? (? 2 x) (? 2 x))
;
; Unification result: (((? 2 x) aull dewitt) ((? 1 rest-2) (onion square) 5) ((? 1 person-2) aull dewitt) ((? 1 rest-1) (onion square) 5) ((? 1 town) . slumerville) ((? 1 person-1) aull dewitt) ((? someone) ? 1 person-2) ((? p1) ? 1 person-1))
;
; 4.3.2) Evaluating not
;
; Since same? will return 'always-true and not will negate that, the whole and will return 'failed
; for this query.

(load "book-code/ch4-query")
(initialize-data-base microshaft-data-base)

; The idea is that now every rule application has it's own scope, much like
; the application procedure in our previous evaluator always had a new scope
; in which it was evaluated. The parent of that scope is the query-frame
; that was passsed as argument to the apply-a-rule.

(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-frame (make-frame '() query-frame)))
    ;(display "\nClean frame")(display clean-frame)
    (let ((unify-result
           (unify-match query-pattern
                        (conclusion rule)
                        clean-frame)))
      ;(display "\nUnified result: ")(display unify-result)
      (if (eq? unify-result 'failed)
          the-empty-stream
          (qeval (rule-body rule)
                 (singleton-stream unify-result))))))

; Procedures that deal with the new environment structure.
; A frame is now a pair of (bindings, parent-frame)
(define (make-frame bindings parent)
  (cons bindings parent))

(define (frame-bindings frame) (car frame))

(define (frame-parent frame) (cdr frame))

; extend adds a local binding and keeps the parent frame
(define (extend variable value frame)
  (make-frame (cons (make-binding variable value) (frame-bindings frame))
              (frame-parent frame)))

; binding-in-frame recurses to the parent frame if a local binding
; was not found for a variable, until there is no parent frame, in
; which case it returns #f
(define (binding-in-frame variable frame)
  (let ((local-assoc (assoc variable (frame-bindings frame))))
    (if local-assoc
      local-assoc
      (let ((parent (frame-parent frame)))
        (if (not (null? parent))
          (binding-in-frame variable parent)
          #f)))))

; Testing
;(define root (make-frame '((a . 1)) '()))
;(define child (extend 'b 2 (make-frame '() root)))
;
;(binding-in-frame 'a root)
;(binding-in-frame 'b root)
;
;(binding-in-frame 'a child)
;(binding-in-frame 'b child)

; We need to override query-driver-loop just so we can initialize the
; empty frame with the expected environment structure
(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base.")
           (query-driver-loop))
          (else
           (newline)
           (display output-prompt)
           (display-stream
            (stream-map
             (lambda (frame)
               (instantiate q
                            frame
                            (lambda (v f)
                              (contract-question-mark v))))
             (qeval q (singleton-stream (make-frame '() '())))))
           (query-driver-loop)))))


; Kick off the evaluator
(query-driver-loop)

; Testing

(lives-near ?p1 ?someone)
; => (lives-near (aull dewitt) (reasoner louis))
; => (lives-near (aull dewitt) (bitdiddle ben))
; => (lives-near (reasoner louis) (aull dewitt))
; => (lives-near (reasoner louis) (bitdiddle ben))
; => (lives-near (hacker alyssa p) (fect cy d))
; => (lives-near (fect cy d) (hacker alyssa p))
; => (lives-near (bitdiddle ben) (aull dewitt))
; => (lives-near (bitdiddle ben) (reasoner louis))

(and (lives-near ?p1 ?someone)
     (lives-near ?p2 ?someone))
; => (and (lives-near (aull dewitt) (reasoner louis)) (lives-near (aull dewitt) (reasoner louis)))
; => (and (lives-near (aull dewitt) (bitdiddle ben)) (lives-near (aull dewitt) (bitdiddle ben)))
; => (and (lives-near (reasoner louis) (aull dewitt)) (lives-near (reasoner louis) (aull dewitt)))
; => (and (lives-near (reasoner louis) (bitdiddle ben)) (lives-near (reasoner louis) (bitdiddle ben)))
; => (and (lives-near (hacker alyssa p) (fect cy d)) (lives-near (hacker alyssa p) (fect cy d)))
; => (and (lives-near (fect cy d) (hacker alyssa p)) (lives-near (fect cy d) (hacker alyssa p)))
; => (and (lives-near (bitdiddle ben) (aull dewitt)) (lives-near (bitdiddle ben) (aull dewitt)))
; => (and (lives-near (bitdiddle ben) (reasoner louis)) (lives-near (bitdiddle ben) (reasoner louis)))

(assert!
  (rule (lives-near-two ?p1 ?p2)
      (and (lives-near ?p1 ?someone)
           (lives-near ?p2 ?someone))))

(lives-near-two ?p1 ?p2)
; => (lives-near-two (aull dewitt) (aull dewitt))
; => (lives-near-two (aull dewitt) (aull dewitt))
; => (lives-near-two (reasoner louis) (reasoner louis))
; => (lives-near-two (reasoner louis) (reasoner louis))
; => (lives-near-two (hacker alyssa p) (hacker alyssa p))
; => (lives-near-two (fect cy d) (fect cy d))
; => (lives-near-two (bitdiddle ben) (bitdiddle ben))
; => (lives-near-two (bitdiddle ben) (bitdiddle ben))
