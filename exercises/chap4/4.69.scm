; Representing (great)^n - grandson relationships

; Load this file and drop yourself in the REPL with:
; $ cat 4.69.scm - | mit-scheme

(load "book-code/ch4-query")

(initialize-data-base '())
(query-driver-loop)

; Let's include the mentioned relations in the database

(assert! (son Adam Cain))
(assert! (son Cain Enoch))
(assert! (son Enoch Irad))
(assert! (son Irad Mehujael))
(assert! (son Mehujael Methushael))
(assert! (son Methushael Lamech))
(assert! (wife Lamech Ada))
(assert! (son Ada Jabal))
(assert! (son Ada Jubal))

(assert! (rule (same ?x ?x)))

; Rules we defined on exercise 4.63:

(assert!
  (rule (grandparent ?G ?S)
    (or (and (son ?G ?P) (son ?P ?S))
        (and (wife ?G ?wife) (son ?wife ?P) (son ?P ?S))
        (and (son ?G ?P) (wife ?P ?wife) (son ?wife ?S)))))

(assert!
  (rule (family ?wife ?husband ?son)
    (and (son ?wife ?son)
         (wife ?husband ?wife))))

; Step 1 from hint: store ((grand grandson) Adam Irad) in our database

(assert! ((great grandson) Adam Irad))

; Step 2: write a rule that determines if a list ends in grandson

(assert!
  (rule (ends-with-grandson (?head . ?tail))
    (ends-with-grandson ?tail)))

(assert!
  (rule (ends-with-grandson (grandson))))

; Testing

(ends-with-grandson (a b grandson))
; => (ends-with-grandson (a b grandson))

(ends-with-grandson (a b grandson c))
; => No answer

; Step 3: Write the rule

(assert!
  (rule ((great . ?rest) ?G ?S)
    (and (ends-with-grandson ?rest)
         (or (and (or (son ?P ?S)
                      (family ?wife ?P ?S))
                  (?rest ?G ?P))
             (and (or (son ?GG ?G)
                      (family ?wife ?GG ?G))
                  (or (son ?P ?S)
                      (family ?wife ?P ?S))
                  ((great . ?rest) ?GG ?P))))))

; Testing

; 1.
((great grandson) ?g ?ggs)
; => ((great grandson) adam irad)
; => ((great grandson) mehujael jubal)
; => ((great grandson) mehujael jabal)
; => ((great grandson) irad lamech)
; => ((great grandson) enoch methushael)
; => ((great grandson) cain mehujael)

; Note: I couldn't find many correct answers online. Most of them just do:
;(assert!
;  (rule ((great . ?rest) ?G ?S)
;    (and (ends-with-grandson ?rest)
;         (and (son ?P ?S)
;              (?rest ?G ?P)))))

; Which yield only:
; => ((great grandson) adam irad)
; Since it won't go up nor down in the family tree and neither
; account for wife-husband-son relationships.

; 2. (Uncomment to test, since it hangs ;P)
;(?relationship Adam Irad)
; => ((great grandson) adam irad)
; Hangs.

; The program hangs because it enters an infinite loop, given
; ?relationship will match agains (great . ?rest) and the recursive
; assertion will also match it again, resulting in the same assertion
; as the original one, causing the program to loop.

; 3.
(?relationship Adam Jubal)
; => ((great great great great great grandson) adam jubal)
