; Handling adjectives in noun and verb phrases

; Load this file and drop yourself in the REPL with:
; $ cat 4.48.scm - | mit-scheme

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

; 1. Handling adjectives in noun phrases

; Let's create a list of adjectives
(define adjectives '(adjective good blue dark hot))

; Let's modify `parse-noun-phrase` to handle possible adjectives
(define (parse-adjective-noun-phrase)
  (amb (parse-simple-noun-phrase)
       (list 'adjective-noun-phrase
             (parse-word articles)
             (parse-word adjectives)
             (parse-word nouns))))

; Now `parse-noun-phrase` uses `parse-adjective-noun-phrase` instead
; of `parse-simple-noun-phrase`
(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend (list 'noun-phrase
                             noun-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-adjective-noun-phrase)))

; Let's try it
(parse '(the dark cat sleeps))
; (sentence (adjective-noun-phrase (article the) (adjective dark) (noun cat)) (verb sleeps))

(parse '(the cat sleeps))
; (sentence (simple-noun-phrase (article the) (noun cat)) (verb sleeps))

; 1. Handling adverbs in verb phrases
(define adverbs '(adverb well hard weekly seriously))

(define (parse-adverb-verb-phrase)
  (amb (parse-word verbs)
       (list 'adverb-verb-phrase
             (parse-word verbs)
             (parse-word adverbs))))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-adverb-verb-phrase)))

; Trying it
(parse '(the cat sleeps well))
; (sentence (simple-noun-phrase (article the) (noun cat)) (adverb-verb-phrase (verb sleeps) (adverb well)))

(parse '(the cat sleeps))
; (sentence (simple-noun-phrase (article the) (noun cat)) (verb sleeps))

; Trying it all together
(parse '(the dark cat sleeps well in the class))
; (sentence
;   (adjective-noun-phrase
;     (article the)
;     (adjective dark)
;     (noun cat))
;   (verb-phrase
;     (adverb-verb-phrase
;       (verb sleeps)
;       (adverb well))
;   (prep-phrase
;     (prep in)
;     (simple-noun-phrase
;       (article the)
;       (noun class)))))

