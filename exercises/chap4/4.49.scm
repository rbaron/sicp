; Generating phrases instead of parsing

; Load this file and drop yourself in the REPL with:
; $ cat 4.49.scm - | mit-scheme

; BEGIN CODE COPY

(load "book-code/ch4-mceval.scm")
(load "book-code/ch4-ambeval.scm")

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

(define (cadr l) (car (cdr l)))
(define (cddr l) (cdr (cdr l)))

; Let's modify `parse-word` so `parse` _generates_ instead of parses phrases
(define (parse-word word-list)
  (require (not (null? (cdr word-list))))
  (amb (list (car word-list) (cadr word-list))
       (parse-word (cons (car word-list) (cddr word-list)))))

(parse '())

; (sentence (simple-noun-phrase (article the) (noun student)) (verb studies))

try-again
; (sentence (simple-noun-phrase (article the) (noun student)) (verb-phrase (verb studies) (prep-phrase (prep for) (simple-noun-phrase (article the) (noun student)))))

try-again

; (sentence (simple-noun-phrase (article the) (noun student)) (verb-phrase (verb-phrase (verb studies) (prep-phrase (prep for) (simple-noun-phrase (article the) (noun student)))) (prep-phrase (prep for) (simple-noun-phrase (article the) (noun student)))))

try-again
; (sentence (simple-noun-phrase (article the) (noun student)) (verb-phrase (verb-phrase (verb-phrase (verb studies) (prep-phrase (prep for) (simple-noun-phrase (article the) (noun student)))) (prep-phrase (prep for) (simple-noun-phrase (article the) (noun student)))) (prep-phrase (prep for) (simple-noun-phrase (article the) (noun student)))))

try-again
; (sentence (simple-noun-phrase (article the) (noun student)) (verb-phrase (verb-phrase (verb-phrase (verb-phrase (verb studies) (prep-phrase (prep for) (simple-noun-phrase (article the) (noun student)))) (prep-phrase (prep for) (simple-noun-phrase (article the) (noun student)))) (prep-phrase (prep for) (simple-noun-phrase (article the) (noun student)))) (prep-phrase (prep for) (simple-noun-phrase (article the) (noun student)))))

try-again
; (sentence (simple-noun-phrase (article the) (noun student)) (verb-phrase (verb-phrase (verb-phrase (verb-phrase (verb-phrase (verb studies) (prep-phrase (prep for) (simple-noun-phrase (article the) (noun student)))) (prep-phrase (prep for) (simple-noun-phrase (article the) (noun student)))) (prep-phrase (prep for) (simple-noun-phrase (article the) (noun student)))) (prep-phrase (prep for) (simple-noun-phrase (article the) (noun student)))) (prep-phrase (prep for) (simple-noun-phrase (article the) (noun student)))))

; As we can see, the generator gets stuck into a "the student
; studies for the student for the student for the student..." recursion.
