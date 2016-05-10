; Implementing `ramb`

; Load this file and drop yourself in the REPL with:
; $ cat 4.50.scm - | mit-scheme

(load "book-code/ch4-mceval.scm")
(load "book-code/ch4-ambeval.scm")

; Helper procedure: given a list, it returns a randomly picked element
; and the list with the element removed
(define (pick-and-remove-random lst)
  (define (pick-at lst-head lst-tail index)
    (if (= index 0)
      (cons (car lst-tail) (append lst-head (cdr lst-tail)))
      (pick-at (cons (car lst-tail) lst-head) (cdr lst-tail) (- index 1))))
  (pick-at '() lst (random (length lst))))

; Analyzer for the new ramb form
(define (analyze-ramb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            (let* ((picked-removed (pick-and-remove-random choices))
                   (picked (car picked-removed))
                   (others (cdr picked-removed)))
              (picked env
                        succeed
                        (lambda ()
                          (try-next others))))))
      (try-next cprocs))))

(define (ramb? exp)
  (tagged-list? exp 'ramb))

; Installing the ramb form
(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((amb? exp) (analyze-amb exp))
        ((ramb? exp) (analyze-ramb exp))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))


; BEGIN CODE COPY

(define the-global-environment (setup-environment))
(driver-loop)

(define (require p)
  (if (not p) (amb)))

(define *unparsed* '())

(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))

(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define prepositions '(prep for to in by with))

(define (parse-sentence)
  (list 'sentence
         (parse-noun-phrase)
         (parse-verb-phrase)))

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))

; Noun phrase is something like "the cat" or "the cat in the class"
(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend (list 'noun-phrase
                             noun-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

; Prepositional phrase is something like "in the class"
(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

; Verb phrase is something like "eats" or "eats in the class with the cat"
(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

; END CODE COPY

; Testing

(ramb 1 2 3 4)
; => 4

try-again
; => 2

try-again
; => 1

try-again
; => 3

try-again
; => ;;; There are no more values of
; => (ramb 1 2 3 4)

; This can help Alyssa's problem from exercise 4.49 in that it avoids getting
; stuck the initial recursion. Let's see:

(define (cadr l) (car (cdr l)))
(define (cddr l) (cdr (cdr l)))

; Let's modify `parse-word` so `parse` _generates_ instead of parses phrases
(define (parse-word word-list)
  (require (not (null? (cdr word-list))))
  (ramb (list (car word-list) (cadr word-list))
       (parse-word (cons (car word-list) (cddr word-list)))))

(parse '())
; (sentence (simple-noun-phrase (article the) (noun cat)) (verb lectures))

try-again
; (sentence (simple-noun-phrase (article the) (noun cat)) (verb-phrase (verb lectures) (prep-phrase (prep for) (simple-noun-phrase (article the) (noun student)))))

try-again
; (sentence (simple-noun-phrase (article the) (noun cat)) (verb-phrase (verb-phrase (verb lectures) (prep-phrase (prep for) (simple-noun-phrase (article the) (noun student)))) (prep-phrase (prep in) (simple-noun-phrase (article a) (noun student)))))

try-again
; (sentence (simple-noun-phrase (article the) (noun cat)) (verb-phrase (verb-phrase (verb-phrase (verb lectures) (prep-phrase (prep for) (simple-noun-phrase (article the) (noun student)))) (prep-phrase (prep in) (simple-noun-phrase (article a) (noun student)))) (prep-phrase (prep for) (simple-noun-phrase (article the) (noun professor)))))

try-again
; (sentence (simple-noun-phrase (article the) (noun cat)) (verb-phrase (verb-phrase (verb-phrase (verb-phrase (verb lectures) (prep-phrase (prep for) (simple-noun-phrase (article the) (noun student)))) (prep-phrase (prep in) (simple-noun-phrase (article a) (noun student)))) (prep-phrase (prep for) (simple-noun-phrase (article the) (noun professor)))) (prep-phrase (prep in) (simple-noun-phrase (article a) (noun professor)))))
