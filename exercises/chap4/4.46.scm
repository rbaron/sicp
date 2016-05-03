; Why won't our parsing program work, should the amb evaluator
; evaluate arguments from right to left?

; Let's take a look at this bit:
(define (parse-sentence)
  (list 'sentence
         (parse-noun-phrase)
         (parse-verb-phrase)))

; The `parse-sentence` procedure constructs a list containing the
; return values of calls to `parse-noun-phrase` and `parse-verb-phrase`.

; When we designed our parsing program, we relied on the fact that the structure
; we were looking for was something like:

; "The cat eats in the class",

; which expect initially a noun phrase _followed_ by a verb phrase.

; If we evaluate the operands the list inside `parse-sentence` from right to left,
; that is, looking first for a _verb_ phrase and _then_ for a noun phrase, our
; program would fail to parse the verb phrase, since it happens _after_ the noun
; phrase.
