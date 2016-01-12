; Load this file and drop yourself in the REPL with:
; $ cat 4.43.scm - | mit-scheme

(load "book-code/ch4-mceval.scm")
(load "book-code/ch4-ambeval.scm")

(define the-global-environment (setup-environment))
(driver-loop)

(define (require p)
  (if (not p) (amb)))

(define (xor clause1 clause2)
  (not (eq? clause1 clause2)))

(define (or clause1 clause2)
  (if clause1 true clause2))

(define (map f lst)
  (if (null? lst) (list)
      (cons (f (car lst)) (map f (cdr lst)))))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (pick-father so-that fathers)
  (if (null? fathers)
    (error "Cannot pick such father")
    (if (so-that (car fathers))
      (car fathers)
      (pick-father so-that (cdr fathers)))))

(define (solutions)
  (let ((moore (list 'mary 'lorna))
        (downing (list (amb 'gabrielle 'lorna 'rosalind) 'melissa))
        (hall (list (amb 'gabrielle 'lorna 'melissa) 'rosalind))
        (barnacle (list 'melissa 'gabrielle))
        (parker (list (amb 'gabrielle 'lorna 'rosalind) 'mary)))

      (let ((fathers (list moore downing hall barnacle parker)))
        (require (distinct? (map car fathers)))

        (let ((gabrielles-father
              (pick-father (lambda (f) (eq? (car f) 'gabrielle)) fathers)))
          (require (eq? (car (cdr gabrielles-father)) (car parker)))

          fathers))))

(solutions)
; => ((mary lorna) (lorna melissa) (gabrielle rosalind) (melissa gabrielle) (rosalind mary))

; In order, that's moore, downing, hall, barnacle, parker. That means lorna's father
; is cel. Downing!

;try-again
; There are no more values of (solutions)


; Now if we remove the information that 'mary is moore's daughter:
(define (solutions)
  (let ((moore (list (amb 'mary 'gabrielle 'rosalind) 'lorna))
        (downing (list (amb 'mary 'gabrielle 'lorna 'rosalind) 'melissa))
        (hall (list (amb 'mary 'gabrielle 'lorna) 'rosalind))
        (barnacle (list 'melissa 'gabrielle))
        (parker (list (amb 'gabrielle 'lorna 'rosalind) 'mary)))

      (let ((fathers (list moore downing hall barnacle parker)))
        (require (distinct? (map car fathers)))

        (let ((gabrielles-father
              (pick-father (lambda (f) (eq? (car f) 'gabrielle)) fathers)))
          (require (eq? (car (cdr gabrielles-father)) (car parker)))

          fathers))))

(solutions)
; ((mary lorna) (lorna melissa) (gabrielle rosalind) (melissa gabrielle) (rosalind mary))

try-again
; ((gabrielle lorna) (rosalind melissa) (mary rosalind) (melissa gabrielle) (lorna mary))

try-again
; There are no more values of (solutions)

; => There are 2 solutions. Lorna's father can be cel. Downing or Mr. Parker.

