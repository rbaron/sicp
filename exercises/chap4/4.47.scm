; Load this file and drop yourself in the REPL with:
; $ cat 4.47.scm - | mit-scheme

; The exercise asks what would happen if we changed the definition of
; parse-verb-phrase from:

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

; To:

(define (parse-verb-phrase)
  (amb (parse-word verbs)
       (list 'verb-phrase
             (parse-verb-phrase)
             (parse-prepositional-phrase))))

; Let's manually evaluate both forms and then test the actual program.

; Original `parse-verb-phrase`:

; (parse '(the professor lectures to the student))
; (sentence (parse-noun-phrase) (parse-verb-phrase))
; (sentence
;   (amb (simple-noun-phrase (article the professor))
;        (...))
;   (parse-verb-phrase))
; (sentence
;   (amb (simple-noun-phrase (article the professor))
;        (...))
;   (amb (verb lectures) ; Dead end
;        (maybe-extend
;          (verb-phrase (verb lectures)
;                       (parse-prepositional-phrase)))))
; (sentence
;   (amb (simple-noun-phrase (article the professor))
;        (...))
;   (amb (verb lectures) ; Dead end
;        (maybe-extend
;          (verb-phrase (verb lectures)
;                       (prep-phrase
;                         (preposition the)
;                         (amb (simple-noun-phrase (article the) (noun student))
;                              (...)))))))
; (sentence
;   (amb (simple-noun-phrase (article the professor)) ; first possible backtrack point
;        (...))
;   (amb (verb lectures) ; Dead end
;        (amb
;          (verb-phrase (verb lectures)
;                       (prep-phrase
;                         (preposition the)
;                         (amb (simple-noun-phrase
;                                (article the)
;                                (noun student)) ; This satisfies all requires!
;                              (...))))
;          (...))))

; Modified `parse-verb-phrase`

; (parse '(the professor lectures to the student))
; (sentence (parse-noun-phrase) (parse-verb-phrase))
; (sentence
;   (amb (simple-noun-phrase (article the professor))
;        (...))
;   (parse-verb-phrase)))
; (sentence
;   (amb (simple-noun-phrase (article the professor))
;        (...))
;   (amb (verb lectures) ; Dead end
;        (verb-phrase
;           (amb (verb lectures)
;                (...))
;           (prep-phrase
;             (preposition to)
;             (amb (simple-noun-phrase
;                    (article the)
;                    (noun student)) ; This satisfies all requires!
;                  (...))))))

; It means the modified version will work (at least once!)

; What if we invert the order of the amb expressions in the modified version?

(define (parse-verb-phrase)
  (amb (list 'verb-phrase
             (parse-verb-phrase)
             (parse-prepositional-phrase))
       (parse-word verbs)))

; (parse '(the professor lectures to the student))
; (sentence (parse-noun-phrase) (parse-verb-phrase))
; (sentence
;   (amb (simple-noun-phrase (article the professor))
;        (...))
;   (parse-verb-phrase)))
; (sentence
;   (amb (simple-noun-phrase (article the professor))
;        (...))
;   (amb (verb-prase
;          (amb (verb-phrase
;                 (amb (verb-phrase
;                        (amb (verb-phrase
;                               (amb ...

; This will hang.

; In the second case, `parse-verb-phrase` returns, ambiguously, either
; the parsed verb phrase or the parsed verb phrase extended by a prepositional
; phrase. If the sentence has two prepositional phrases, the parsed sentence
; can be in the wrong order.

; Let's test both cases:

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


; Here's version 1:
(parse '(the professor lectures to the student))
;(sentence
;  (simple-noun-phrase
;    (article the)
;    (noun professor))
;  (verb-phrase
;    (verb lectures)
;    (prep-phrase
;      (prep to)
;      (simple-noun-phrase
;        (article the)
;        (noun student)))))

; Here's version 2:
(define (parse-verb-phrase)
  (amb (parse-word verbs)
       (list 'verb-phrase
             (parse-verb-phrase)
             (parse-prepositional-phrase))))

(parse '(the professor lectures to the student))
;(sentence
;  (simple-noun-phrase
;    (article the)
;    (noun professor))
;  (verb-phrase
;    (verb lectures)
;    (prep-phrase
;      (prep to)
;      (simple-noun-phrase
;        (article the)
;        (noun student)))))a

; Here's version 2 with the inverted amb order:
(define (parse-verb-phrase)
  (amb
       (list 'verb-phrase
             (parse-verb-phrase)
             (parse-prepositional-phrase))
      (parse-word verbs)))

;(parse '(the professor lectures to the student))
; This will hang!
