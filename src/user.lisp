;;; This file include entry operators for the users.

(in-package :clon)

(defmacro schedule-lambda (&key name form time)
  ;; Basic example
  ;;
  ;; (schedule-lambda
  ;;  :name "demo"
  ;;  :form ((print (get-universal-time)))
  ;;  :time (:second '(member 0 10 20 30 40 50)))
  `(let* (;; create a stateless schedule
          (schedule (make-typed-cron-schedule
                     ,@time))
          ;; create a scheduler that remembers the last scheduled time
          (scheduler (make-scheduler schedule)))
     ;; schedule a function as a timer
     (schedule-function (lambda () ,@form)
                        scheduler
                        :name nil
                        :immediate t
                        :ignore-skipped t
                        :thread t)))

(defmacro dry-run (n &key time)
  "Dry-run N times for the schedule defined by the schedule
definition TIME."
  ;; basic example
  ;;
  ;; (dry-run 8 :time (:day-of-wek '(member 0 1 2)))
  `(let ((schedule (make-typed-cron-schedule ,@time)))
     (let ((next-time (next-time schedule)))
       (dotimes (x ,n)
         (format t "~s~%" (local-time:universal-to-timestamp next-time))
         (setf next-time (next-time schedule :init-time next-time))))))
