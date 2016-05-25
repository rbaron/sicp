; Delayed vs. eager evaluation of rules

; Louis is interested in evaulating the difference between the
; used simple-query procedure:

(load "book-code/ch4-query")

; Delayed simple-query:
;(define (simple-query query-pattern frame-stream)
;  (stream-flatmap
;   (lambda (frame)
;     (stream-append-delayed
;      (find-assertions query-pattern frame)
;      (delay (apply-rules query-pattern frame))))
;   frame-stream))

; and an alternative one without the delay call:

; Eager simple-query:
;(define (simple-query query-pattern frame-stream)
;  (stream-flatmap
;   (lambda (frame)
;     (stream-append (find-assertions query-pattern frame)
;                    (apply-rules query-pattern frame)))
;   frame-stream))

; The potentially undesirable behavior will come from applying the procedure
; (apply-rules query-pattern frame), instead of delaying it.
; One example of such behavior happens when applying rules will lead to
; an infinite loop. For example:

(initialize-data-base '())
(query-driver-loop)

(assert! (brother b1 b2))

(assert!
  (rule (brother ?b2 ?b1)
        (brother ?b1 ?b2)))

; For queries such as (brother ?a ?b), we will match both the inserted
; assertion (brother b1 b2) _and_ the rule (brother ?b2 ?b1). The rule,
; when matched, will recurse on `qeval` using the rule-body
; (brother ?b1 ?b2) as query. This, in turn, will also match the assertion
; _and_ the rule, leading to an infinite loop. In the delayed case, we actually
; match the assertion first, delaying the application of the rule. When
; consuming the resulting stream, we force the application of the rule,
; leading to, again, the match of the assertion and delayed match of the
; rule. So, in the delayed case, an infinite list of matches will be generated.

; In the eager case, the application of the rule will lead to an infinite loop,
; which will never produce any match.

; Uncomment and check that the program hangs
; (brother b1 ?p2)

; In the disjunction case, the problem is more evident. In order to evaluate
; a truth-y disjunction, it suffices to evaluate the first true clause. For
; instance:

(assert! (rule (maybe-inf)
  (or (brother b1 b2)
      (maybe-inf))))

; Uncomment to print an infinite stream of (maybe-inf)s. (Use the original
; simple-query)
; (maybe-inf)

; Analogously to the lazy simple-query, this will print an inifite stream
; of (maybe-inf)s. The eager alternative:

;(define (disjoin disjuncts frame-stream)
;  (if (empty-disjunction? disjuncts)
;      the-empty-stream
;      (interleave
;       (qeval (first-disjunct disjuncts) frame-stream)
;       (disjoin (rest-disjuncts disjuncts) frame-stream))))

; Will cause the program to hang and not print anything, sice it will
; recurse into the rule-application until the stack overflows.
