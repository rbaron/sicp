; See 1.14_recursion_tree.jpg for the recursion tree.
;
; Space analysis
; ==============
;
; To evaluate the space needed, let's analyze what kind of information we need to keep while visiting
; these states. The call stack looks something like this:
;
; (cc 11 5)
; (+ (cc -39 5) (cc 11 4))
; (+ 0 (+ (cc -14 4) (cc 11 3)))
; (+ 0 (+ 0 (+ (cc 1 3) (cc 11 2))))
; (+ 0 (+ 0 (+ (+ (cc -9 3) (cc 1 2)) (cc 11 2))))
; (+ 0 (+ 0 (+ (+ 0 (cc 1 2)) (cc 11 2))))
; (+ 0 (+ 0 (+ (+ 0 (+ (cc -4 2) (cc 1 1))) (cc 11 2))))
; (+ 0 (+ 0 (+ (+ 0 (+ 0 (+ (cc 0 1) (cc 1 0)))) (cc 11 2))))
; (+ 0 (+ 0 (+ (+ 0 (+ 0 (+ 1 0))) (cc 11 2))))
; (+ 0 (+ 0 (+ 1 (cc 11 2))))
; (+ 0 (+ 0 (+ 1 (+ (cc 6 2) (cc 11 1)))))
; (+ 0 (+ 0 (+ 1 (+ (+ (cc 1 2) (cc 1 1)) (cc 11 1)))))
; (+ 0 (+ 0 (+ 1 (+ (+ (+ (cc -4 2) (cc 1 1)) (cc 1 1)) (cc 11 1)))))
; (+ 0 (+ 0 (+ 1 (+ (+ (+ 0 (+ (cc 0 1) (cc 1 0))) (cc 1 1)) (cc 11 1)))))
; (+ 0 (+ 0 (+ 1 (+ (+ (+ 0 (+ 1 0)) (cc 1 1)) (cc 11 1)))))
; (+ 0 (+ 0 (+ 1 (+ (+ (+ 0 1) (cc 1 1)) (cc 11 1)))))
; (+ 0 (+ 0 (+ 1 (+ (+ 1 (cc 1 1)) (cc 11 1)))))
; (+ 0 (+ 0 (+ 1 (+ (+ 1 (+ (cc 0 1) (cc 1 0))) (cc 11 1)))))
; (+ 0 (+ 0 (+ 1 (+ (+ 1 (+ 1 0)) (cc 11 1)))))
; (+ 0 (+ 0 (+ 1 (+ (+ 1 1) (cc 11 1)))))
; (+ 0 (+ 0 (+ 1 (+ 2 (cc 11 1)))))
;   ...
;
; We can see that when a state is visited, we only need to keep track of the call stack with
; the nodes above it in the tree. This means that the worst case in space happens when the deepest
; node is visited. The maximum height will happen when the most amount of coins is used. That happens
; when all the change is returned in the one cent coins. So we can say that the space growth
; belongs to theta(change).
;
; Time analysis
; =============
;
; If we look at the right hand side of the image (1.14_recursion_tree.jpg), we can see an interesting
; pattern for the call of (cc 11 1). It follows the pattern:
;
; (cc n 1)
; (+ (cc n-1 1) (cc n 0))
; (+ (+ (cc n-2 1) (cc n-1 0)) (cc n 0))
; (+ (+ (+ (cc n-3 1) (cc n-2 0)) (cc n-1 0)) (cc n 0))
; (+ (+ (+ (+ (cc n-4 1) (cc n-3 0)) (cc n-2 0)) (cc n-1 0)) (cc n 0))
;   ...
;
; Which means we'll keep going deeper on the left tree while n-i*1 > 0. That will happen when i == n.
; Once that happens, we we'll "backtrack" on the tree (only to find invalid right-hand side calls).
; We'll have 2*n + 1 calls. So, we can say that T((cc n 1)) = theta(n).
;
; Let's look at what happens when we have calls to (cc n 2):
;
; (cc n 2)
; (+ (cc n-5 2) (cc n 1))
; (+ (+ (cc n-10 2) (cc n-5 1)) (cc n 1))
; (+ (+ (+ (cc n-15 2) (cc n-10 1)) (cc n-5 1)) (cc n 1))
; (+ (+ (+ (+ (cc n-20 2) (cc n-15 1)) (cc n-10 1)) (cc n-5 1)) (cc n 1))
;   ...
;
; We can see that each call to (cc n 2) branches left to (cc n-5 2) and right to (cc n 1).
; We will keep going down while n-5*i > 0. Thus, we're gonna have ~n/5 left branches.
; As we can also see, each left branch will also have a matching right branch as
; (cc n-5*i 1), which takes theta(n) time. Thus,
;
; T((cc n 2)) = theta(n) * T((cc n 1)) = theta(n^2)
;
; Following that thought for calls to (cc n 3), we get:
;
; (cc n 3)
; (+ (cc n-10 3) (cc n 2))
; (+ (+ (cc n-20 3) (cc n-10 2)) (cc n 2))
; (+ (+ (+ (cc n-30 3) (cc n-20 2)) (cc n-10 2)) (cc n 2))
;   ...
;
; Again, we're gonna have ~n/10 branches, and each of those have calls to (cc n-10*i 2),
; which take T((cc n-10*i 2)) = theta(n^2). Thus:
;
; T((cc n 3)) = theta(n) * theta(n^2) = theta(n^3)
;
; Going further, we'll have that calls to (cc n k) take theta(n^k) time. Specifically for the
; exercise, we have k = 5.
;
;
; Note: A balanced binary tree with height h has 2^(h+1)-1 nodes. Proof: Sum of geometric progression
; with r = 2, a0 = 1.
