# Profiling notes

Used SBCL's [Deterministic Profiler](http://www.sbcl.org/1.0/manual/Deterministic-Profiler.html#Deterministic-Profiler)

### profile

```lisp
(sb-profile:profile "TWO-OPT")
(time (test-optimise))
(sb-profile:report)

### reset profiler

```
(sb-profile:reset)
(sb-profile:unprofile "TWO-OPT")
```

### 1st pass results

had to abort due to time. partial results:

```lisp
starting distance = 2842782.6950
Evaluation took:
  1342.116 seconds of real time
  1339.664000 seconds of total run time (638.720000 user, 700.944000 system)
  [ Run times consist of 3.616 seconds GC time, and 1336.048 seconds non-GC time. ]
  99.82% CPU
  3 forms interpreted
  4,165,458,340,680 processor cycles
  1 page fault
  36,189,786,240 bytes consed
  
  before it was aborted by a non-local transfer of control.
  
; Evaluation aborted on NIL.
```

  seconds  |     gc     |     consed     |     calls     |  sec/call  |  name  
--------------------------------------------------------------------
    48.222 |      2.088 | 20,088,303,376 |   251,111,636 |   0.000000 | MOVE-COST
    11.274 |      1.520 | 16,072,198,832 | 1,004,466,308 |   0.000000 | DISTANCE-SQUARED
     5.635 |      0.000 |      1,474,560 |        91,416 |   0.000062 | #:FIND-MOVE
     4.028 |      0.000 |              0 |   125,751,555 |   0.000000 | WRAP
     0.081 |      0.008 |     24,197,760 |         9,882 |   0.000008 | LINE-TO-POINT
     0.073 |      0.012 |     23,972,528 |         9,882 |   0.000007 | #:LINE-TO-POINT
     0.027 |      0.000 |              0 |        30,297 |   0.000001 | REVERSE-SUBSEQ
     0.013 |      0.000 |        720,896 |        19,764 |   0.000001 | DISTANCE
     0.011 |      0.000 |              0 |        31,131 |   0.000000 | #:REVERSE-SUBSEQ
     0.010 |      0.000 |              0 |             1 |   0.009730 | #:OPTIMISE
     0.007 |      0.000 |        688,128 |             2 |   0.003652 | #:TOUR-DISTANCE
     0.001 |      0.000 |        360,368 |         9,882 |   0.000000 | MAKE-POINT
     0.000 |      0.000 |        294,912 |        19,764 |   0.000000 | #:DISTANCE
     0.000 |      0.000 |         32,544 |             1 |   0.000000 | #:TEST-OPTIMISE
     0.000 |      0.000 |      1,296,928 |             1 |   0.000000 | #:LOAD-POINTS
     0.000 |      0.000 |        360,240 |         9,882 |   0.000000 | #:MAKE-POINT
     0.000 |      0.000 |         63,440 |   251,111,637 |   0.000000 | TRY-MOVE
     0.000 |      0.000 |              0 |             1 |   0.000000 | OPTIMISE
     0.000 |      0.000 |      2,290,912 |        60,092 |   0.000000 | FIND-MOVE
     0.000 |      0.000 |      1,060,832 |             1 |   0.000000 | LOAD-POINTS
     0.000 |      0.000 |              0 |             1 |   0.000000 | TEST-OPTIMISE
     0.000 |      0.000 |        589,824 |             2 |   0.000000 | TOUR-DISTANCE
--------------------------------------------------------------------
    69.380 |      3.628 | 36,217,906,080 | 1,632,733,138 |            | Total




