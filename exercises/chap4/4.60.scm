; Repeated pairs

; Load this file and drop yourself in the REPL with:
; $ cat 4.60.scm - | mit-scheme

; Alyssa argues that the query:

; (lives-near ?person-1 ?person-2)

; causes the same pair of people to apear twice. E.g.:

; (lives-near (Hacker Alyssa P) (Fect Cy D))
; (lives-near (Fect Cy D) (Hacker Alyssa P))

; Why does this happen?

; => It happens because the interpreter instantiates every possible
;    permutation of the entries. That means [1, 2] and [2, 1] are
;    considered separately.
;    At the interpreter level, we could instead consider combinations
;    of the entries, so that only one of the two permutations would be
;    tested for satisfiability.
;    At the "user level", we could write a simple assertion to check
;    Whether two entries are "sorted". Here's how:


; Let's define a helper procedure that checks whether two
; lists of symbols are lexicographically sorted:
(define (are-symbols-sorted? symbols1 symbols2)
  (define (are-strings-sorted? strings1 strings2)
    (if (null? strings1)
      #t
      (if (null? strings2)
        #f
        (let ((n1 (car strings1))
              (n2 (car strings2)))
          (if (string=? n1 n2)
            (strings-in-order? (cdr strings1) (cdr strings2))
            (string<? n1 n2))))))

  (are-strings-sorted? (map symbol->string symbols1)
                       (map symbol->string symbols2)))

(load "book-code/ch4-query")

(initialize-data-base microshaft-data-base)
(query-driver-loop)

(assert! (rule (lives-near-unique ?person1 ?person2)
  (and (lives-near ?person1 ?person2)
       (lisp-value are-symbols-sorted? ?person1 ?person2))))

; Testing

; Old way:
(lives-near ?p1 ?p2)
; => (lives-near (aull dewitt) (reasoner louis))
; => (lives-near (aull dewitt) (bitdiddle ben))
; => (lives-near (reasoner louis) (aull dewitt))
; => (lives-near (reasoner louis) (bitdiddle ben))
; => (lives-near (hacker alyssa p) (fect cy d))
; => (lives-near (fect cy d) (hacker alyssa p))
; => (lives-near (bitdiddle ben) (aull dewitt))
; => (lives-near (bitdiddle ben) (reasoner louis))

; New way:
(lives-near-unique ?p1 ?p2)
; => (lives-near-unique (aull dewitt) (reasoner louis))
; => (lives-near-unique (aull dewitt) (bitdiddle ben))
; => (lives-near-unique (fect cy d) (hacker alyssa p))
; => (lives-near-unique (bitdiddle ben) (reasoner louis))
