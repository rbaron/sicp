; Implementing the union of sets.

; Given procedures

; `equal?` from ex. 2.54
(define (equal? lista listb)
  (if (and (null? lista) (null? listb))
    #t
    (let (a (car lista))
         (b (car listb))
      (cond ((and (pair? a) (pair? b))
              (and (equal? a b))
                   (equal? (cdr lista) (cdr listb)))
            ((and (not (pair? a)) (not (pair? b)))
              (if (eq? a b)
                (equal? (cdr lista) (cdr listb))
                #f))
            (else #f)))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

; Solution

(define (union s1 s2)
  (if (null? s1)
    s2
    (if (element-of-set? (car s1) s2)
      (union (cdr s1) s2)
      (cons (car s1) (union (cdr s1) s2)))))

; Testing

(define s1 (list '(1 2) 3))
(define s2 (list 3 '(4) 5 '(1 2)))

(union s1 s2)
; => (3 (4) 5 (1 2))
