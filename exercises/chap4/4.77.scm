; Fixing `not` behavior when there are unbound variables
; in the frame

; Load this file and drop yourself in the REPL with:
; $ cat 4.77.scm - | mit-scheme

; NOTE: this is a limited solution, serving only as a proof of concept
; about using delayed filters to solve the problem with `not` and `lisp-value`.

(load "book-code/ch4-query")
(initialize-data-base microshaft-data-base)


(define (negate2 operands frame-stream)
  (stream-map
    (lambda (frame)
      ; The idea is to append a filter function to every frame. It should be
      ; run once all cariables are bound in the frame. That is, once every
      ; other operation was applied.
      (make-delayed-filter
        frame
        (lambda (frame)
          (if (stream-null? (qeval (negated-query operands)
                                   (singleton-stream frame)))
              (singleton-stream frame)
              the-empty-stream))))
    frame-stream))

(define (make-delayed-filter frame filter-func)
  (list 'delayed-filter frame filter-func))

(define (is-delayed-filter? frame)
  (and (pair? frame) (eq? 'delayed-filter (car frame))))

(define (delayed-frame frame) (cadr frame))

(define (delayed-filter frame) (caddr frame))

; Applies all delayed filters, should a frame be delayed
(define (apply-filters frame-stream)
  (stream-flatmap
    (lambda (frame)
      (if (is-delayed-filter? frame)
        ((delayed-filter frame) (delayed-frame frame))
        (singleton-stream frame)))
  frame-stream))

; Simply call (apply-filters frame-stream) once all
; conjoins were evalyated
(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      (apply-filters frame-stream)
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts)
                      frame-stream))))

; Added a new conditional: if the frame is a delayed filter, we replace
; that frame with the resulting frame from applying the simple query.
(define (simple-query query-pattern frame-stream)
  (stream-flatmap
    (lambda (frame)
      (if (is-delayed-filter? frame)
        (stream-map
          (lambda (new-frame)
            ; Here we generate a new frame stream, preserving the delayed
            ; filter
            (make-delayed-filter new-frame (delayed-filter frame)))
          (stream-append-delayed
            (find-assertions query-pattern (delayed-frame frame))
            (delay (apply-rules query-pattern (delayed-frame frame)))))

        (stream-append-delayed
         (find-assertions query-pattern frame)
         (delay (apply-rules query-pattern frame)))))
   frame-stream))

(put 'not2 'qeval negate2)

(query-driver-loop)

(and (supervisor ?x ?y)
     (not2 (job ?x (computer programmer))))
; => (and (supervisor (aull dewitt) (warbucks oliver)) (not2 (job (aull dewitt) (computer programmer))))
; => (and (supervisor (cratchet robert) (scrooge eben)) (not2 (job (cratchet robert) (computer programmer))))
; => (and (supervisor (scrooge eben) (warbucks oliver)) (not2 (job (scrooge eben) (computer programmer))))
; => (and (supervisor (bitdiddle ben) (warbucks oliver)) (not2 (job (bitdiddle ben) (computer programmer))))
; => (and (supervisor (reasoner louis) (hacker alyssa p)) (not2 (job (reasoner louis) (computer programmer))))
; => (and (supervisor (tweakit lem e) (bitdiddle ben)) (not2 (job (tweakit lem e) (computer programmer))))

(and (not2 (job ?x (computer programmer)))
     (supervisor ?x ?y))
; => (and (not2 (job (aull dewitt) (computer programmer))) (supervisor (aull dewitt) (warbucks oliver)))
; => (and (not2 (job (cratchet robert) (computer programmer))) (supervisor (cratchet robert) (scrooge eben)))
; => (and (not2 (job (scrooge eben) (computer programmer))) (supervisor (scrooge eben) (warbucks oliver)))
; => (and (not2 (job (bitdiddle ben) (computer programmer))) (supervisor (bitdiddle ben) (warbucks oliver)))
; => (and (not2 (job (reasoner louis) (computer programmer))) (supervisor (reasoner louis) (hacker alyssa p)))
; => (and (not2 (job (tweakit lem e) (computer programmer))) (supervisor (tweakit lem e) (bitdiddle ben)))


; For reference, here's the wrong output with the regular not:

(and (supervisor ?x ?y)
     (not (job ?x (computer programmer))))
; => (and (supervisor (aull dewitt) (warbucks oliver)) (not (job (aull dewitt) (computer programmer))))
; => (and (supervisor (cratchet robert) (scrooge eben)) (not (job (cratchet robert) (computer programmer))))
; => (and (supervisor (scrooge eben) (warbucks oliver)) (not (job (scrooge eben) (computer programmer))))
; => (and (supervisor (bitdiddle ben) (warbucks oliver)) (not (job (bitdiddle ben) (computer programmer))))
; => (and (supervisor (reasoner louis) (hacker alyssa p)) (not (job (reasoner louis) (computer programmer))))
; => (and (supervisor (tweakit lem

(and (not (job ?x (computer programmer)))
     (supervisor ?x ?y))
; => Empty
