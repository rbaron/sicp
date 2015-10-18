; Implementing deques

; We can use our queue implementation as a base for implementing
; new tricks. The exercise asks us to represent deques as pairs.

(define (make-deque)
  (cons '() '()))

(define (front-ptr deque) (car deque))

(define (rear-ptr deque) (cdr deque))

(define (set-front-ptr! deque item) (set-car! deque item))

(define (set-rear-ptr! deque item) (set-cdr! deque item))

; In order to accomplish tasks in O(1), we now need to be able
; to traverse our structure from left to right _and_ from right
; to left. This means every "node" in the chain should keep
; pointers to the next _and_ previous "node", instead of just to
; the next one, as it was with our queue implementation.

; In the queue implementation, we could get away with a simple pair
; consisting of a value and a pointer to the next element. We now
; need to implement a double-linked list, so I will abstract that
; implementation with a "node" structure:

(define (make-node value prev next)
  (list value prev next))

(define (value node) (car node))
(define (prev node) (cadr node))
(define (next node) (caddr node))
(define (set-prev! node ptr) (set-car! (cdr node) ptr))
(define (set-next! node ptr) (set-car! (cddr node) ptr))

; We can now start another abstraction layer. Using the above
; implementation of deque and node , we can define our operations.

(define (empty-deque? deque) (null? (front-ptr deque)))

(define (front-deque deque) (car (front-ptr deque)))

(define (rear-deque deque) (car (rear-ptr deque)))

(define (front-insert-deque! deque item)
  (cond ((empty-deque? deque)
          (let ((new-node (make-node item '() '())))
            (set-front-ptr! deque new-node)
            (set-rear-ptr! deque new-node)))
        (else
          (let ((new-node (make-node item '() (front-ptr deque))))
            (set-prev! (front-ptr deque) new-node)
            (set-front-ptr! deque new-node)))))

(define (rear-insert-deque! deque item)
  (cond ((empty-deque? deque)
          (let ((new-node (make-node item '() '())))
            (set-front-ptr! deque new-node)
            (set-rear-ptr! deque new-node)))
        (else
          (let ((new-node (make-node item '(rear-ptr deque) '())))
            (set-next! (rear-ptr deque) new-node)
            (set-rear-ptr! deque new-node)))))

(define (front-delete-deque! deque)
  (set-front-ptr! deque (next (front-ptr deque))))

(define (rear-delete-deque! deque)
  (set-rear-ptr! deque (prev (rear-ptr deque))))


; Testing

(define d1 (make-deque))

(empty-deque? d1)
; => #t

(front-insert-deque! d1 1)

(empty-deque? d1)
; => #f

(front-deque d1)
; => 1

(rear-deque d1)
; => 1

(front-insert-deque! d1 2)

(front-deque d1)
; => 2

(rear-deque d1)
; => 1

(rear-delete-deque! d1)

(front-deque d1)
; => 2

(rear-deque d1)
; => 2
