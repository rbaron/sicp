; This exercise asks us to develop a system for handling mobiles.

; Given procedures
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; 2.29 a - Writing selectors
(define (left-branch mobile) (car mobile))

(define (right-branch mobile) (car (cdr mobile)))

(define (branch-length branch) (car branch))

(define (branch-structure branch) (car (cdr branch)))

; Testing
(define branch1 (make-branch 1 10))
(define branch2 (make-branch 2 20))

(branch-length (left-branch (make-mobile branch1 branch2)))     ; => 1
(branch-structure (left-branch (make-mobile branch1 branch2)))  ; => 10
(branch-length (right-branch (make-mobile branch1 branch2)))    ; => 2
(branch-structure (right-branch (make-mobile branch1 branch2))) ; => 20

; 2.29 b - Writing a `total-weight` procedure
(define (total-weight mobile)
  (if (not (pair? mobile))
    mobile
    (+ (total-weight (branch-structure (left-branch mobile)))
       (total-weight (branch-structure (right-branch mobile))))))

; Testing
(define mobile1 (make-mobile branch1 branch2))

(total-weight mobile1) ; => 30

; 2.29 c - Checking if the mobile is balanced with a `balanced?` procedure
(define (balanced? mobile)
  (cond
    ((not (pair? mobile))
      #t)
    (else
      (let ((lb (left-branch mobile))
            (rb (right-branch mobile)))
        (and
          (= (* (branch-length lb) (total-weight (branch-structure lb)))
             (* (branch-length rb) (total-weight (branch-structure rb))))
          (and (balanced? (branch-structure lb))
               (balanced? (branch-structure rb))))))))

(define unbalanced-mobile
  (make-mobile
    (make-branch 11
      (make-mobile
        (make-branch 2 4)
        (make-branch 1 8)))
    (make-branch 6 20)))

(define balanced-mobile
  (make-mobile
    (make-branch 10
      (make-mobile
        (make-branch 2 4)
        (make-branch 1 8)))
    (make-branch 6 20)))

(balanced? unbalanced-mobile) ; => #f
(balanced? balanced-mobile)   ; => #t

; 2.29 d - Changing the representation of mobiles

; Suppose we now represent mobiles as:
(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))

; How much of the program do we have to change so we can still use
; the `total-weight` and `balanced?` procedures?

; The "left" (or `car`) selectors can be left as they are, since
(car (cons 1 2)) ; => 1

; is the same as
(car (list 1 2)) ; => 1

; We just need to take care of the "right" (or `cdr`) selectors, since
(cdr (cons 1 2)) ; => 2

; is not the same as
(cdr (list 1 2)) ; => (2)

; Let's rewrite the "right" procedures:
(define (right-branch mobile) (cdr mobile))
(define (branch-structure branch) (cdr branch))

; With the new selectors in scope, let's redefine our mobiles
; and call the same old `balanced?` procedure (which in turn
; calls the `total-weight` procedure:

(define unbalanced-mobile
  (make-mobile
    (make-branch 11
      (make-mobile
        (make-branch 2 4)
        (make-branch 1 8)))
    (make-branch 6 20)))

(define balanced-mobile
  (make-mobile
    (make-branch 10
      (make-mobile
        (make-branch 2 4)
        (make-branch 1 8)))
    (make-branch 6 20)))

(balanced? unbalanced-mobile) ; => #f
(balanced? balanced-mobile)   ; => #t

; Conclusion: by abstracting `mobile` and `branch`, we made the
; program easier to maintain and modify!
