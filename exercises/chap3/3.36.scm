; Analyzing the enviroment for setting a value to a connector

; When evaluating the following 3 procedures:

; (define a (make-connector))
; (define b (make-connector))
; (set-value! a 10 'user)

; At some point, the following call will be executed:

; (for-each-except setter inform-about-value constraints)

; Let's figure out what's the enviroment structure in which
; that call is executed.

; Let's break it in steps. Executing:

; (define a (make-connector))
; (define b (make-connector))

; Will produce the following structure:

;                +-----------------------------+ <--------+
;                |        set-value: ...       |          |
; Global env ->  |  a: +-+              b: +--------+     |
;                |       |                     |    |     |
;                |       | for-each-except:... |    |     |
;                +-------------+---------------+    |     |
;                        |     ^                    |     |
;                        |     |                    |     |
;                        |     |                    |     |
;                        |     |                    |     |
;                        |   +-+--------------+     |   +-+--------------+
;                        |   |                |     |   |                |
;                        |   | value: #f      |     |   | value: #f      |
;                        |   | informant: #f  |     |   | informant: #f  |
;                        |   | constraints:   |     |   | constraints:   |
;                        |   | set-my-value:  |     |   | set-my-value:  |
;                        |   | dispatch:      |     |   | dispatch:      |
;                        |   +--+-------------+     |   +--+-------------+
;                        |      |                   |      |
;                        |      |   E1              |      |     E2
;                        v      |                   v      |
;                      +-+-+    |                 +-+-+    |
;                   +---| |-----+              +---| |-----+
;                   |  +---+                   |  +---+
;                   v                          v
;           parameter: request         parameter: request
;           body: (request body)       body: (request body)


; Now, calling

; (set-value! a 10 'user)

; Will call `set-value` with a new enviroment E3, having the global enviroment as its
; enclosing enviroment. It will then call the `dispatch` procedure with a new frame
; E4. This will return the internal procedure `set-my-value`, which will be executed
; in a new frame E5 (pointing to E1).

; Inside `set-my-value`, the following mutators will be called:

; (set! value 10)
; (set! informant 'user)

; Which will set the values of `value` and `informant` in E1.

; Now, the relevant call:

; (for-each-except setter inform-about-value constraints)

; We can see that `for-each-except` is defined in the global
; enviroment, so the new enviroment for its execution, E6,
; will point to the global one.

; We'll have:

;                        +-----------------------------+ <--------+
;                        |       set-my-value: ...     |          |
;         Global env ->  |  a: +-+              b: +--------+     |
;                        |       |                     |    |     |
;       +---------------->       | for-each-except: ...|    |     |
;       |                +-------------+---------------+    |     |
;       |                        |     ^                    |     |
;       |                        |     |                    |     |
;   +---+-----------------+      |     |                    |     |
;   | exception: 'user    |      |     |                    |     |
;   | procedure: inform...|      |   +-+--------------+     |   +-+--------------+
;   | list: '()           |      |   |                |     |   |                |
;   +---------------------+      |   | value: 10      |     |   | value: #f      |
;                                |   | informant: 'us |     |   | informant: #f  |
; E5 (exec of for-each-except)   |   | constraints:   |     |   | constraints:   |
;                                |   | set-my-value:  |     |   | set-my-value:  |
;                                |   | dispatch:      |     |   | dispatch:      |
;                                |   +--^--+----------+     |   +--^-------------+
;                                |      |  ^                |      |
;                                |      |  |   E1           |      |     E2
;                                v      |  |                v      |
;                              +-+-+    |  |              +-+-+    |
;                           +---| |-----+  |           +---| |-----+
;                           |  +---+       |           |  +---+
;                           v              |           v
;                   parameter: request     |   parameter: request
;                   body: (request body)   |   body: (request body)
;                                          |
;                                      +---+-----------------+
;                                      |  new-val: 10        |
;                                      |  setter: 'user      | E5 (exec of set-my-value)
;                                      +---------------------+





