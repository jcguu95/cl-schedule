;;; This file include entry operators for the users.

(in-package :cl-schedule)

(defvar *schedules* nil)

(defmacro schedule! (&key name form time)
  "Schedule the FORM to be run with NAME and time spec TIME."
  `(let* (;; create a stateless schedule
          (schedule (make-typed-cron-schedule ,@time))
          ;; create a scheduler that remembers the last scheduled time
          (scheduler (make-scheduler schedule)))
     ;; schedule a function as a timer
     (push (schedule-function (lambda () ,@form)
                               scheduler
                               :name ,name
                               :immediate t
                               :ignore-skipped t
                               :thread t)
           *schedules*)))

(defmacro dry-run (n &optional schedule-definition)
  "Dry-run N times for the schedule defined by the
SCHEDULE-DEFINITION."
  (let ((x (gensym)))
    `(let ((schedule (make-typed-cron-schedule ,@schedule-definition)))
       (let ((next-time (next-time schedule)))
         (dotimes (,x ,n)
           (format t "~s~%" (local-time:universal-to-timestamp next-time))
           (setf next-time (next-time schedule :init-time next-time)))))))

;; API with schedules
;; TODO Native inspection, pause.. etc.
(defun all-schedules () *schedules*)
(defun unschedule (schedule)
  (trivial-timers:unschedule-timer schedule)
  (setf *schedules* (remove schedule *schedules*)))
