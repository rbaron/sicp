; Queues as procedures with local state


; Let's complete the given skeleton:

; (define (make-queue)
;   (let ((front-ptr ...)
;         (rear-ptr ...))
;     <definitions of internal procedures>
;     (define (dispatch m) ...)
;     dispatch))

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))

  (define (set-front-ptr! item) (set! front-ptr item))

  (define (set-rear-ptr! item) (set! rear-ptr item))

  (define (empty-queue?) (null? front-ptr))

  (define (dispatch m)
    (cond ((eq? m 'set-front-ptr!) set-front-ptr!)
          ((eq? m 'set-rear-ptr!) set-rear-ptr!)
          ((eq? m 'front-ptr) front-ptr)
          ((eq? m 'rear-ptr) rear-ptr)
          ((eq? m 'empty-queue?) empty-queue?)
          (else (error "Invalid operation -- MAKE-QUEUE"))))

    dispatch))

; We could also make this procedures internal to `make-queue`, but
; I chose to create another abstraction barrier between how queues
; are implemented and how they're used. Since the following three
; procedures are defined in terms of "queue primitives", we can
; leave them separate.

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

; Exposing as functions

(define (front-ptr queue) (queue 'front-ptr))

(define (rear-ptr queue) (queue 'rear-ptr))

(define (set-front-ptr! queue item) ((queue 'set-front-ptr!) item))

(define (set-rear-ptr! queue item) ((queue 'set-rear-ptr!) item))

(define (empty-queue? queue) ((queue 'empty-queue?)))


; Testing:
(define q1 (make-queue))

(empty-queue? q1)
; => #t

(insert-queue! q1 'a)

(empty-queue? q1)
; => #f

(front-queue q1)
; => a

(insert-queue! q1 'b)

(front-queue q1)
; => a

(delete-queue! q1)

(front-queue q1)
; => b

(delete-queue! q1)

(empty-queue? q1)
; => #t
