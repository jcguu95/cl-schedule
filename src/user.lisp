;;; This file include entry operators for the users.

(in-package :cl-schedule)

(defvar *schedules* nil)

;; TODO Document the slots (in docstrings).
;; TODO Document the benefits of this class (in readme).
(defclass schedule ()
  ((name  :initarg :name
          :accessor name)
   (form  :initarg :form
          :accessor form)
   (time  :initarg :time
          :accessor time-spec)
   (timer :initarg :timer
          :accessor timer)
   (memo  :initarg :memo
          :accessor memo)
   (cron-schedule :initarg :cron-schedule
                  :accessor cron-schedule)))

(defmacro schedule! (&key name form time (memo ""))
  "Schedule the FORM to be run with NAME and time spec TIME."
  `(let* ((cron-schedule            ; create a stateless schedule
            (make-typed-cron-schedule ,@time))
          (scheduler                ; create a scheduler that remembers the last scheduled time
            (make-scheduler cron-schedule))
          ;; Starts a thread that runs the form.
          (timer     (schedule-function (lambda () ,@form)
                                        scheduler
                                        :name ,name
                                        :immediate t
                                        :ignore-skipped t
                                        :thread t))
          ;; Package information for easier inspection later.
          (schedule  (make-instance 'schedule
                                    :name         ,name
                                    :memo         ,memo
                                    :form        ',form
                                    :time        ',time
                                    :timer         timer
                                    :cron-schedule cron-schedule)))
     (push schedule *schedules*)))

(defmacro dry-run (n &optional schedule-definition)
  ;; TODO This does not have to be a macro.
  "Dry-run N times for the schedule defined by the
SCHEDULE-DEFINITION."
  (let ((x (gensym)))
    `(let ((schedule (make-typed-cron-schedule ,@schedule-definition))) ; TODO In order to resolve the macro, some work has to be done here..
       (let ((next-time (next-time schedule)))
         (dotimes (,x ,n)
           (format t "~s~%" (local-time:universal-to-timestamp next-time))
           (setf next-time (next-time schedule :init-time next-time)))))))

(defun all-schedules (&optional pred)
  ;; TODO Document an example of PRED in readme.
  "Return the list of schedules that satisfy the predicate PRED."
  (if pred
      (remove-if-not pred *schedules*)
      *schedules*))

(defmethod next-time ((schedule schedule)
                      &key
                        (init-time (get-universal-time))
                        allow-now-p
                        (limit *default-next-time-limit*))
  ;; Document its usage in readme.
  (local-time:universal-to-timestamp
   (next-time (cron-schedule schedule)
              :init-time   init-time
              :allow-now-p allow-now-p
              :limit       limit)))

;; TODO Implement: Pause a schedule.

(defun unschedule (schedule)
  "Unschedule SCHEDULE and return SCHEDULE as the primary value."
  (trivial-timers::unschedule-timer schedule)
  (setf *schedules* (remove schedule *schedules*))
  (values schedule *schedules*))
