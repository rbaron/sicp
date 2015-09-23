; Message passing

; Implementing `make-from-mag-ang` as a procedure.

(define (make-from-mag-ang mag ang)
  (define (dispatch op)
    (cond ((eq? op 'real) (* mag (cos ang)))
          ((eq? op 'real) (* mag (sin ang)))
          ((eq? op 'mag) mag)
          ((eq? op 'ang) ang)
          (else
            (error "Unknown op -- MAKE-FROM-MAG-ANG" op)))))
