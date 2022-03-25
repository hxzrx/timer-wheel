# 原始Timer-Wheel库的不足之处
1. 不能调度循环任务;
2. 不能直接设置UT时任务;
3. 时轮线程未做错误处理, 一旦出现错误会影响所有未执行的任务.
4. 关闭时轮时应该给未执行的任务进行标注并清空时间片槽位, 仍然需要计时器的状态槽位(取消, 等待, 运行, 超时, 完成), 目前只有unscheduled状态, 在remaining槽位. 这种关闭实际上是重置.
5. 不能说是不足, 在某些情况下, 计时任务可在多线程环境下执行, 但所有任务都运行与时轮线程, 若任务有IO瓶颈则很容易导致后面任务超时.
6. 同样也不是不足, 但允许荷载函数接收参数会更方便.
7. with-timeout功能, 在多久后调度某任务.

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
