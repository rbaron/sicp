; This exercise asks us to redefine the procedure `count-leaves` in
; terms of `accumulate`.

; Given procedure
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (n) (if (pair? n) (count-leaves n) 1)) t)))


; Testing
(define x (cons (list 1 2) (list 3 4)))
(define x2 (list x x))

(count-leaves x)  ; => 4
(count-leaves x2) ; => 8
