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

# Testing
I've tested on Windows with SBCL 1.3 and CCL 1.11.  I could get a timer wheel running with a resolution of 20 milliseconds without overruns (Nothing else going on).  If you start loading up the garbage collector, I'm guessing that you'll start getting overruns.  Test to determine what works.

# TODO
- Add a hierarchical timer wheel
- Add OS specific timer back-ends?
- HW timer backends?
