# Clon: A cron-like scheduling library in Common Lisp

A basic example is given below. For more, see
`./doc/example.lisp`.

``` common-lisp
(let* (;; create a stateless schedule
       (schedule (make-typed-cron-schedule
                  :second '(member 0 10 20 30 40 50)))
       ;; create a scheduler that remembers the last scheduled time
       (scheduler (make-scheduler schedule)))
  ;; schedule a function as a timer
  (schedule-function (lambda ()
                       (print (get-universal-time))) 
                     scheduler))

;; unschedule the timer returned by SCHEDULE-FUNCTION:
(trivial-timers:unschedule-timer *)
```
