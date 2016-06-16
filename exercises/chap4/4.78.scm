; Implementing the query evaluator using the amb evaluator

; NOTE: it took me a while to figure out how to even approach this
; problem. I ended up finding a way which, once found, seemed
; "obvious". I chose not to implement the full query language
; because it proved to be a hard problem for me; Instead, I implemented
; part of it to serve as a proof of concept. The rest of the language
; (namely: rules and procedures such as conjoin, disjoin etc), can be
; implemented in a similar fashion, given enough time to think ;P

; Load this file and drop yourself in the REPL with:
; $ cat 4.78.scm - | mit-scheme

(load "book-code/ch4-mceval.scm")
(load "book-code/ch4-ambeval.scm")

(define the-global-environment (setup-environment))
(driver-loop)

(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

; The database is simply a list of assertions
(define database '(
  (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
  (job (Bitdiddle Ben) (computer wizard))
  (salary (Bitdiddle Ben) 60000)

  (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
  (job (Hacker Alyssa P) (computer programmer))
  (salary (Hacker Alyssa P) 40000)
  (supervisor (Hacker Alyssa P) (Bitdiddle Ben))

  (address (Fect Cy D) (Cambridge (Ames Street) 3))
  (job (Fect Cy D) (computer programmer))
  (salary (Fect Cy D) 35000)
  (supervisor (Fect Cy D) (Bitdiddle Ben))

  (address (Tweakit Lem E) (Boston (Bay State Road) 22))
  (job (Tweakit Lem E) (computer technician))
  (salary (Tweakit Lem E) 25000)
  (supervisor (Tweakit Lem E) (Bitdiddle Ben))

  (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
  (job (Reasoner Louis) (computer programmer trainee))
  (salary (Reasoner Louis) 30000)
  (supervisor (Reasoner Louis) (Hacker Alyssa P))

  (supervisor (Bitdiddle Ben) (Warbucks Oliver))

  (address (Warbucks Oliver) (Swellesley (Top Heap Road)))
  (job (Warbucks Oliver) (administration big wheel))
  (salary (Warbucks Oliver) 150000)

  (address (Scrooge Eben) (Weston (Shady Lane) 10))
  (job (Scrooge Eben) (accounting chief accountant))
  (salary (Scrooge Eben) 75000)
  (supervisor (Scrooge Eben) (Warbucks Oliver))

  (address (Cratchet Robert) (Allston (N Harvard Street) 16))
  (job (Cratchet Robert) (accounting scrivener))
  (salary (Cratchet Robert) 18000)
  (supervisor (Cratchet Robert) (Scrooge Eben))

  (address (Aull DeWitt) (Slumerville (Onion Square) 5))
  (job (Aull DeWitt) (administration secretary))
  (salary (Aull DeWitt) 25000)
  (supervisor (Aull DeWitt) (Warbucks Oliver))

  (can-do-job (computer wizard) (computer programmer))
  (can-do-job (computer wizard) (computer technician))

  (can-do-job (computer programmer)
              (computer programmer trainee))

  (can-do-job (administration secretary)
              (administration big wheel))
))

; qeval now receives an ambiguous frame instead of a frame stream.
; The mental model is that `frame` here actually takes multiple values
; _at once_.

; Down the road, we're gonna `require` that some assertions from the database
; "match" the query in those frames, thus killing some of the execution branches.
; The amb evaluator should automatically handle the backtracking so
; a new assertion will be tested until there are no more values.

; Note that I only implemented simple queries. Implementing the handling of
; procedures and rules are left as an exercise to the reader's reader ;P
(define (qeval query frame)
  (simple-query query frame))

(define (simple-query query-pattern frame)
  (find-assertions query-pattern frame))

; find-assertions now fetches data from the database and
; `require` that that data pattern-matches the query in
; a given frame
(define (find-assertions pattern frame)
  (let ((datum (an-element-of database)))
    (let ((new-frame (pattern-match pattern datum frame)))
      (require (not (eq? new-frame 'failed)))
      new-frame)))

; pattern-match and extend-if-consistent stay unchanged
(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame)
        ((var? pat) (extend-if-consistent pat dat frame))
        ((and (pair? pat) (pair? dat))
         (pattern-match (cdr pat)
                        (cdr dat)
                        (pattern-match (car pat)
                                       (car dat)
                                       frame)))
        (else 'failed)))

(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (pattern-match (binding-value binding) dat frame)
        (extend var dat frame))))

; Bindings abstractions stay unchanged
(define (make-binding variable value)
  (cons variable value))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

(define (binding-in-frame variable frame)
  (assoc variable frame))

(define (extend variable value frame)
  (cons (make-binding variable value) frame))

(define (var? exp)
  (tagged-list? exp '?))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (and p1 p2)
  (if p1 p2 false))

; Testing:

; 1. Finding all people whose job is (computer ?something)

(qeval '(job (? who) (computer (? something))) (amb '()))
; => (((? something) . wizard) ((? who) bitdiddle ben))
try-again
; => (((? something) . programmer) ((? who) hacker alyssa p))
try-again
; => (((? something) . programmer) ((? who) fect cy d))
try-again
; => (((? something) . technician) ((? who) tweakit lem e))
try-again
; => ;;; There are no more values of
; => (qeval (quote (job (? who) (computer (? something)))) (amb (quote ())))

; 2. Finding all people who are supervised by Warbucks Oliver
(qeval '(supervisor (? supervisee) (Warbucks Oliver)) (amb '()))
; => (((? supervisee) bitdiddle ben))
try-again
; => (((? supervisee) scrooge eben))
try-again
; => (((? supervisee) aull dewitt))
try-again
; => ;;; There are no more values of
; => (qeval (quote (supervisor (? supervisee) (warbucks oliver))) (amb (quote ())))
