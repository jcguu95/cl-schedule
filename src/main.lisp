(in-package :cl-schedule)

(defun make-schedule (&key time func name (memo ""))
  "The official API to make a schedule."
  ;; Each schedule better has a unique name among all schedules
  ;; in *schedules*.
  (when (all-schedules-with-name name)
    (setf name (gen-unique-name name)))
  (make-instance 'schedule :func func
                           :name name
                           :memo memo
                           :time time))

(defmethod schedule ((schedule schedule))
  "The official API to schedule a schedule."
  (if (all-schedules-with-name (name schedule))
      (error "The name of the schedule ~a is taken." schedule)
      ;; User should avoid pushing their own schedules into
      ;; *SCHEDULES*.
      (values schedule
              (push schedule *schedules*))))

(defun schedule! (&key time func name (memo ""))
  "The official API to schedule a schedule from spec."
  (reset-global-scheduler)
  (schedule (make-schedule :func func
                           :name name
                           :memo memo
                           :time time)))

(defmethod unschedule! ((schedule schedule))
  (setf *schedules*
        (remove schedule *schedules*)))

(defmethod unschedule! ((pred function))
  (setf *schedules*
        (remove-if pred *schedules*)))

(defun all-schedules-enabled ()
  (remove-if-not #'enabled *schedules*))

(defun all-schedules-disabled ()
  (remove-if #'enabled *schedules*))

(defun all-schedules-with-name (name)
  (remove-if-not (lambda (schedule)
                   (string= name (name schedule)))
                 *schedules*))
