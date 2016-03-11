# Timer Wheel
A portable Common Lisp timer wheel implementation.  A timer wheel provides an efficient mechanism to implement tick-based timer routines.  The portable part of this is the backend using Boreaux Threads to run a background thread operating the wheel.  

## References
For the original reference I used, see "Real-Time Embedded Systems" by Xiaocong Fan, chapter 22.3.
For some background on timers in Linux
- http://www.elinux.org/Kernel_Timer_Systems
- https://lkml.org/lkml/2005/10/19/46.

# Example
```lisp
(asdf:load-system :timer-wheel.examples)

;; Increment and print a counter value every tick with two different counters.
;; A third counter triggers the completion of the test.
(tw.examples:test 100 1500)
```
