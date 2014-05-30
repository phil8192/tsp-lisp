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


