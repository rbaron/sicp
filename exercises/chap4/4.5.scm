; Supporting the alternative `cond` syntax

; The alternative syntax is as follows:

; (cond ((assoc 'b '((a 1) (b 2))) => cadr)
;       (else false))

(define (cond-alternative? clause)
   (eq? (cadr clause) '=>))

(define (cond-alternative-predicate clause)
  (car clause))

(define (cond-alternative-recipient clause)
  (caddr clause))

(define (make-application operation argument)
  (list operation argument))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     ; Modified here
                     ;(sequence->exp (cond-actions first))
                     (cond-clause->exp first)
                     (expand-clauses rest))))))

(define (cond-clause->exp clause)
  (if (cond-alternative? clause)
    (make-application (cond-alternative-recipient clause)
                      (cond-alternative-predicate clause))
    (sequence->exp (cond-actions clause))))
