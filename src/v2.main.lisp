(in-package :cl-schedule-2)

(defun schedule! (&key time func (name "") (memo ""))
  (let ((schedule (make-instance 'schedule
                                 :func func
                                 :name name
                                 :memo memo
                                 :time time)))
    (values schedule
            (push schedule *schedules*))))

;; TODO Pressure Test (I observe that sometimes it skips! Not
;; sure if it is because of log4cl.
