# Test

```lisp
;; (require :sb-sprof)
;; (sb-sprof:with-profiling (:max-samples 1 :report :flat :loop nil) 
;;   (time (test-optimise)))
;;
;; (sb-profile:profile "TWO-OPT")
;; (time (test-optimise))
;; (sb-profile:report)
;;
;; (sb-profile:reset)
;; (sb-profile:unprofile "TWO-OPT")
```
