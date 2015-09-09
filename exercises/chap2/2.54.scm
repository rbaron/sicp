; Implementing `equal?` as a recursive procedure to assert
; the equality between lists

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

; Testing

(equal? '(a b c) '(a b c))
; => #t

(equal? '(a b c) '(a b))
; => #f

(equal? '((a b) c) '((a b) c))
; => #t

(equal? '((a b) c) '((a) c))
; => #f

(equal? '(a (b c) d) '(a (b c) d))
; => #t

(equal? '(a (b c) d) '(a (b) d))
; => #f

(equal? '(a (b c)) '(a (b c) d))
; => #f
