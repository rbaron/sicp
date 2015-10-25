; Can we come up with a scenario in which the deadlock
; avoidance mechanism will fail?

; Suppose we are at a very small restaurant that has only one knife
; and only one fork. We are holding the place with one of our hands,
; so we can pick up the fork and afterwards pick up the knife. Or we
; could pick up the knife first and then pick up the fork.

; If we pick up the knife first and someone else in the mean time
; picks up the fork, we're going to wait until the other resource
; (for us, the fork) is available. The other person, who picked up
; the fork will also wait for the other resource (the knife, which
; we have). In this case, a deadlock would happen.

; The source of this problem is that we have related resources which
; can be accessed by different resources in different orders.
