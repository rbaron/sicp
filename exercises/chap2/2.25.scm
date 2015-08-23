; This exercise asks us to give the combination of `car`s and `cdr`s
; that will extract the symbol `7` from the following lists:

(define l1 (list 1 3 (list 5 7) 9))

(car (cdr (car (cdr (cdr l1))))) ; => 7

(define l2 (list (list 7)))

(car (car l2)) ; => 7

(define l3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

;(cdr (cdr (cdr (cdr (cdr l3)))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l3)))))))))))) ; => 7
