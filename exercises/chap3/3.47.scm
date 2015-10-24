; Implementing semaphores

; a. In terms of mutexes

; The idea is to have a pool of mutexes available
; for acquisition.

(define (make-semaphore size)
  (let ((count-mutex (make-mutex))
        (active 0))

    (define (acquire)
      (count-mutex 'acquire)
      (if (< active size)
        (begin (set! active (+ active 1))
               (count-mutex 'release))
        ; Retry
        (begin (count-mutex 'release))
               (acquire)))

    (define (release)
      (count-mutex 'acquire)
      (set! active (- active 1))
      (count-mutex 'release))

    (define (dispatch msg)
      (cond ((eq? msg 'acquire) acquire)
            ((eq? msg 'release) release)))

    dispatch))


; b. In terms of `test-and-set!`

(define (make-semaphore size)
  (let ((cell (list false))
        (active 0))

    (define (acquire)
      (if (test-and-set! cell)
        ; cell was true - retry
        (acquire)
        (if (< active size)
          (begin (set! active (+ active 1))
                 (clear! cell))
          ; All slots are taken. Retry
          (begin (clear! cell)
                 (acquire)))))

    (define (release)
      (if (test-and-set! cell)
        ; cell was true - retry
        (release)
        (begin (set! active (- active 1))
               (clear! cell))))

    (define (dispatch msg)
      (cond ((eq? msg 'acquire) acquire)
            ((eq? msg 'release) release)))

    dispatch))
