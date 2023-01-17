(in-package :cl-schedule-2)

(defparameter *scheduler-name*
  "cl-schedule:global-scheduler")

(defun global-schedulers ()
  "Return the lists of global schedulers. We identify them by
superficially by thread names."
  (remove-if-not
   (lambda (thread)
     (string= *scheduler-name* (bt:thread-name thread)))
   (bt:all-threads)))

(defun global-scheduler ()
  "Wake up and schedule every (1+ *rounds*) seconds."
  (loop
    (sleep 1)
    (if (> *counter* 0)
        (decf *counter*)
        (progn
          (setf *counter* *rounds*)
          (setf *actions* (make-hash-table))
          (let ((names (mapcar #'name *schedules*)))
            (log:debug "Scheduling for ~a for the next ~a seconds.." names (1+ *rounds*)))
          (loop
            for schedule in *schedules*
            do (loop
                 for time in (dry-run (time-pred schedule)
                                      :init (get-universal-time) :range (1+ *rounds*))
                 do (symbol-macrolet ((time-slot (gethash time *actions*)))
                      (if time-slot
                          (unless (find schedule time-slot)
                            (push schedule time-slot))
                          (setf time-slot (list schedule))))))))))

(defun reset-global-scheduler ()
  (mapc #'bt:destroy-thread (global-schedulers))
  (bt:make-thread #'global-scheduler
                  :name *scheduler-name*))

(reset-global-scheduler)
