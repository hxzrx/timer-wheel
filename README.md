This timer-wheel was forked from <https://github.com/npatrick04/timer-wheel>. The functions were extended a lot and most of which were rewritten as well optimized.

The differences between this lib and the vanilla lib are:
1. Use universal time instead of internal real time.
2. Can schedule with wall time through a timestring.
3. Can Schedule periodically tasks now.
4. Remove locks and use atomic operations instead.

More Detailed test cases were written and all were passed in sbcl and ccl.

A not strict test showed that the new lib could schedule at the rate of about 90M times per second, under my linux vm with sbcl 2.2.3 installed.

# Basic Usage
1. Create a wheel instance with make-wheel.
2. Create timer instances with make-timer.
3. Schedule the timer instanceses with schedule-timer.

# APIs

## function make-wheel
Make a timer wheel with SIZE slots, with a millisecond RESOLUTION, and BACKEND of :BT (bordeaux-threads, the only backend).

lambda list: (&key (size *default-size*) (resolution *default-resolution*) (name (string (gensym "WHEEL-"))) (backend :bt))

## class timer
Class definition of timer class.

### Slots
remaining: ticks left to funcall the callback.

installed-slot: the slot index where this timer locates in the wheel's slots.

callback: a function which accepts two parameters: wheel and timer.

start: The start time of the timer, will convert to universal time in milliseconds in make-timer. Note that the start slot of the timer means it will begin to run after this time, but not mean it should run as soon as the time is fulfilled. Think about a timer is created with a very long time before the current time, the past runs should be discarded or not? There is no perfect solution!

period: the period for the repeatable task, and the period should be exact n-times the resolution of the attached scheduler. So, if scheduler is initialized, the period should be a positive integer for periodical tasks, or nil for non-periodical tasks.

end: the end time of the timer, will convert to universal time in milliseconds in make-timer, for those unbound periodical tasks, this will initialized to most-positive-fixnum.

scheduler: a scheduler this timer attached to, can be re-attached.

result: sometimes one need the returned value of the callback function, and this slot provide a clumsy way to store the value, one should set this slot-value manually within the callback's function body.

scheduled-p: if it's been scheduled, this slot will be set to T.

timeout-p: T shows that is has a timeout when it got ran, NOT in use currently.

status: default :OK, but when a timer is uninstalled, the status will be set to :canceled.

bindings: specials bindings, may be useful in threads, NOT in use currently.

## function make-timer
Return a new timer object.

lambda list: (&key callback scheduler start-time end-time (repeat-times 1 repeat-times-supplied-p) period-in-seconds bindings (name (string (gensym "TIMER-"))))

callback: a function that accepts WHEEL and TIMER arguments.

scheduler: a wheel instance this new timer will attach to.

start-time: the earliest time this timer begins to run (not schedule), start-time accept 3 types of data: 1. nil, will schedule immediately; 2. string, a timestring which can be convert to a universal time, with the machine's timezone if not specified. eg. 2022-03-24 16:28:00, 2022-03-24T16:28:00.000+08:00; 3. an instance of local-time:timestamp.

repeat-times: The times the timer will run, and 1 for non-periodical timer, The slot-value will decf by 1 after each run and stop when it get down to 0. If repeat-times is not supplied but period-in-seconds is supplied, repeats will try to be inferred.

period-in-seconds: This is the interval value for the periodical timer, and nil for the non-periodical timer. But if scheduler is specified, it will convert to the time ticks with respect to the sceduler's resolution. If scheduler is not specified, it will convert to milliseconds and stored in the period slot also. Note that the timer's period in milliseconds should be a common multiple of the resolution of the scheduler, or else an error will be signaled. If repeat-times >= 2, and period-in-seconds is not supplied, the period will be inferenced from start, end, and repeats.

## function attach-scheduler
Attach a timer to a scheduler and (re)set the period. This is useful is a timer's not initialized with a wheel.

lambda list: (timer new-scheduler)

## get-real-period
Return the timers period in milliseconds.

lambda list: ((timer timer))

## method install-timer
Add TIMER to the WHEEL schedule.

lambda list: (wheel timer)

## method reinstall-timer
Add TIMER to the WHEEL schedule again. This method is useful for periodical tasks.

lambda list: (wheel timer)

## method uninstall-timer
Remove TIMER from the WHEEL schedule.

lambda list: (wheel timer)

## function schedule-timer
Schedule a timer with an optional delay-seconds. delay-seconds: The real value of seconds, rounded to the nearest resolution tick.  Return T if the timer is succefully scheduled, and return NIl if it's failed.

lambda list: (wheel timer &optional delay-seconds)

delay-seconds is used for some timer that will be sheduled some time later. For simplicity, if delay-seconds is specified, the start timer of the timer will not take effect. delay-seconds is usually for the kind of timer that is not well designed with many arguments supplied with make-timer. If one want to schedule a timer with wall time, make the timer with make-timer and supply a start-time argument, then called it with schedule-timer and left delay-seconds unsupplied.

## function schedule-timer-simply
Schedule a timer with the scheduler it's attached to.

lambda list: (timer &optional delay-seconds)

## function shutdown-timer-wheel
Notify the wheel thread to terminate, then wait for it.

## macro with-timer-wheel
Execute BODY after initializing WHEEL, then clean up by shutting WHEEL down after leaving the scope.

lambda list: (wheel &body body)

## macro with-timeout
Encapsule BODY into a timer and schedule it with TIMEOUT seconds and return the timer object. TIMEOUT is a positive real number in seconds. SCHEDULER and TIMER are symbols which can be used in BODY as the returned timer's callback's arguments.

lambda list: ((wheel timeout &optional (scheduler (gensym)) (timer (gensym))) &body body)

## parameter *default-resolution*
The default milliseconds resolution of a wheel. Defaults to 100.

## parameter *default-size*
Slots per wheel. Defaults to 100.
For example, a wheel with resolution 100 and size 100 is a ten-seconds-long period scheduler.

## parameter *expired-epsilon*
For those timers which has exipred time specified, *expired-epsilon* makes an reasonable tolerate to schedule them. Defaults to the value of *default-resolution*.

## *wheel-list*
A list to keep all wheels. Defaults to nil.


-------------

(vanilla doc)

# Timer Wheel
A portable Common Lisp timer wheel implementation.  A timer wheel provides an efficient mechanism to implement tick-based timer routines.  The portable part of this is the backend using Boreaux Threads to run a background thread operating the wheel.

Note that this package uses the internal-real-time to manage inter-tick timing intervals.  Thus the correctness of the interval depends on how the specific lisp implementation implements internal-real-time.  For instance, [SBCL currently uses gettimeofday internally](https://sourceforge.net/p/sbcl/mailman/message/35583449/) which means that a leap-second can stop time.

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
