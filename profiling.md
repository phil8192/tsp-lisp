# Profiling notes

Used SBCL's [Deterministic Profiler](http://www.sbcl.org/1.0/manual/Deterministic-Profiler.html#Deterministic-Profiler)

### profile

```lisp
(sb-profile:profile "TWO-OPT")
(time (test-optimise))
(sb-profile:report)
```

### reset profiler

```lisp
(sb-profile:reset)
(sb-profile:unprofile "TWO-OPT")
```

### 1st pass results

had to abort due to time. partial results:

```
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

  seconds  |     gc     |     consed    |    calls    |  sec/call  |  name  
-----------------------------------------------------------------
     7.531 |      0.456 | 4,613,653,632 |  57,654,921 |   0.000000 | MOVE-COST
     6.717 |      0.236 | 3,690,573,968 | 230,639,447 |   0.000000 | DISTANCE-SQUARED
     1.294 |      0.000 |             0 |  28,893,769 |   0.000000 | WRAP
     0.073 |      0.016 |    24,106,400 |       9,882 |   0.000007 | LINE-TO-POINT
     0.013 |      0.000 |             0 |      12,486 |   0.000001 | REVERSE-SUBSEQ
     0.005 |      0.000 |       327,504 |       9,882 |   0.000001 | MAKE-POINT
     0.002 |      0.000 |       720,896 |      19,764 |   0.000000 | DISTANCE
     0.000 |      0.000 |             0 |  57,654,921 |   0.000000 | TRY-MOVE
     0.000 |      0.000 |             0 |           1 |   0.000000 | OPTIMISE
     0.000 |      0.000 |       196,608 |      18,357 |   0.000000 | FIND-MOVE
     0.000 |      0.000 |     1,191,856 |           1 |   0.000000 | LOAD-POINTS
     0.000 |      0.000 |             0 |           1 |   0.000000 | TEST-OPTIMISE
     0.000 |      0.000 |       589,824 |           2 |   0.000000 | TOUR-DISTANCE
-----------------------------------------------------------------
    15.634 |      0.708 | 8,331,360,688 | 374,913,434 |            | Total

estimated total profiling overhead: 272.94 seconds
overhead estimation parameters:
  1.6000001e-8s/call, 7.2800003e-7s total profiling, 2.8e-7s internal profiling
```

### 2nd pass results

After inlining the functions called the most, total time down to 5.3 seconds:

```lisp
(declaim (inline distance-squared))
(declaim (inline move-cost))
(declaim (inline try-move))
(declaim (inline wrap))
```

```
TWO-OPT> (sb-profile:reset)
NIL
TWO-OPT> (time (test-optimise))
starting distance = 2842782.6950
optimised distance = 340852.9544
Evaluation took:
  5.319 seconds of real time
  5.324000 seconds of total run time (5.232000 user, 0.092000 system)
  [ Run times consist of 0.012 seconds GC time, and 5.312 seconds non-GC time. ]
  100.09% CPU
  16,508,141,508 processor cycles
  28,124,896 bytes consed
  
NIL
TWO-OPT> (sb-profile:report)

  seconds  |     gc     |   consed   |  calls  |  sec/call  |  name  
----------------------------------------------------------
     5.080 |      0.000 |  1,474,560 |  91,416 |   0.000056 | FIND-MOVE
     0.069 |      0.012 | 23,854,688 |   9,882 |   0.000007 | LINE-TO-POINT
     0.019 |      0.000 |          0 |  31,131 |   0.000001 | REVERSE-SUBSEQ
     0.011 |      0.000 |  1,550,816 |       1 |   0.011145 | LOAD-POINTS
     0.007 |      0.000 |    688,128 |       2 |   0.003573 | TOUR-DISTANCE
     0.005 |      0.000 |          0 |       1 |   0.004671 | OPTIMISE
     0.000 |      0.000 |    229,248 |   9,882 |   0.000000 | MAKE-POINT
     0.000 |      0.000 |     32,544 |       1 |   0.000000 | TEST-OPTIMISE
     0.000 |      0.000 |    294,912 |  19,764 |   0.000000 | DISTANCE
----------------------------------------------------------
     5.192 |      0.012 | 28,124,896 | 162,080 |            | Total

estimated total profiling overhead: 0.12 seconds
overhead estimation parameters:
  1.6000001e-8s/call, 7.2800003e-7s total profiling, 2.8e-7s internal profiling

These functions were not called:
 DISTANCE-SQUARED MOVE-COST TRY-MOVE WRAP
```
