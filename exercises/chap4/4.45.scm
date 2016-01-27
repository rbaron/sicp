; Load this file and drop yourself in the REPL with:
; $ cat 4.45.scm - | mit-scheme

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

; Verb phrase is something like "eats" or "eats in the class"
(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))


(parse '(the professor lectures to the student in the class with the cat))

;(sentence
;  (simple-noun-phrase (article the) (noun professor))
;  (verb-phrase
;    (verb-phrase
;      (verb-phrase
;        (verb lectures)
;        (prep-phrase
;          (prep to)
;          (simple-noun-phrase (article the) (noun student))))
;      (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class))))
;      (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))

try-again

;(sentence
;  (simple-noun-phrase (article the) (noun professor))
;  (verb-phrase
;    (verb-phrase
;      (verb lectures)
;      (prep-phrase (prep to) (simple-noun-phrase (article the) (noun student))))
;    (prep-phrase
;      (prep in)
;      (noun-phrase
;        (simple-noun-phrase (article the) (noun class))
;        (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))))

try-again

;(sentence
;  (simple-noun-phrase (article the) (noun professor))
;  (verb-phrase
;    (verb-phrase
;      (verb lectures)
;      (prep-phrase
;        (prep to)
;        (noun-phrase
;          (simple-noun-phrase (article the) (noun student))
;          (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class))))))
;    (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))

try-again

;(sentence
;  (simple-noun-phrase (article the) (noun professor))
;  (verb-phrase
;    (verb lectures)
;    (prep-phrase
;      (prep to)
;      (noun-phrase
;        (noun-phrase
;          (simple-noun-phrase (article the) (noun student))
;          (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class))))
;        (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))))

try-again

;(sentence
;  (simple-noun-phrase (article the) (noun professor))
;  (verb-phrase
;    (verb lectures)
;    (prep-phrase
;      (prep to)
;      (noun-phrase
;        (simple-noun-phrase (article the) (noun student))
;        (prep-phrase
;          (prep in)
;          (noun-phrase
;            (simple-noun-phrase (article the) (noun class))
;            (prep-phrase
;              (prep with)
;              (simple-noun-phrase (article the) (noun cat)))))))))

try-again

;;; There are no more values of
; (parse (quote (the professor lectures to the student in the class with the cat)))

(parse '(the cat eats))
(parse '(the cat in the class eats))
