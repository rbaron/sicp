; Serializing one of two procedures

(define x 10)

(define s (make-serializer))

; The call

(parallel-execute (lambda () (set! x ((s (lambda () (* x x))))))
                  (s (lambda () (set! x (+ x 1)))))

; has only one serialized procedure..

; The possible outcomes are:

; 1. P1 gets x = 10 twice, P2 gets x = 10 and sets x to 11, P1 sets
;   x to 10*10 = 100
;
; 2. P1 gets x = 10 twice and sets x to 10*10 = 100, P2 gets x = 100
;   and sets x to 101
;
; 3. P2 gets x = 10 and sets x to 11. P1 gets x = 11 twice and sets
;   x to 121
