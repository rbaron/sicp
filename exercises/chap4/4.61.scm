; `next-to` relation

; Load this file and drop yourself in the REPL with:
; $ cat 4.61.scm - | mit-scheme

(load "book-code/ch4-query")

(initialize-data-base microshaft-data-base)
(query-driver-loop)

; What will the following rules:

(assert!
  (rule (?x next-to ?y in (?x ?y . ?u))))

(assert!
  (rule (?x next-to ?y in (?v . ?z))
        (?x next-to ?y in ?z)))

; Yield in the following cases?

; a. (?x next-to ?y in (1 (2 3) 4))
; => (1 next-to (2 3) in (1 (2 3) 4))
; => ((2 3) next-to 4 in (1 (2 3) 4))

; b. (?x next-to 1 in (2 1 3 1))
; => (2 next-to 1 in (2 1 3 1))
; => (3 next-to 1 in (2 1 3 1))

; Testing

(?x next-to ?y in (1 (2 3) 4))
; => ((2 3) next-to 4 in (1 (2 3) 4))
; => (1 next-to (2 3) in (1 (2 3) 4))

(?x next-to 1 in (2 1 3 1))
; => (3 next-to 1 in (2 1 3 1))
; => (2 next-to 1 in (2 1 3 1))
