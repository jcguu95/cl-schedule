(in-package :cl-schedule-2)

(defun pred<-spec (time-spec)
  (lambda (time)                        ; universal time
    (multiple-value-bind (second minute hour day month year)
        (decode-universal-time time)
      (unless (getf time-spec :year)
        (setf (getf time-spec :year) t))
      (unless (getf time-spec :month)
        (setf (getf time-spec :month) t))
      (unless (getf time-spec :day)
        (setf (getf time-spec :day) t))
      (unless (getf time-spec :hour)
        (setf (getf time-spec :hour) t))
      (unless (getf time-spec :minute)
        (setf (getf time-spec :minute) t))
      (unless (getf time-spec :second)
        (setf (getf time-spec :second) t))
      (and
       ;; TODO Support week-day and timezone?
       (typep year   (getf time-spec :year))
       (typep month  (getf time-spec :month))
       (typep day    (getf time-spec :day))
       (typep hour   (getf time-spec :hour))
       (typep minute (getf time-spec :minute))
       (typep second (getf time-spec :second))))))

(defun dry-run (time-spec
                &key
                  (init (get-universal-time))
                  (range 100))
  "Return the list of times (in universal format) from init
to (init+range) that satisfies TIME-SPEC."
  ;; (when (consp time-predicate)
  ;;   (setf time-predicate (pred<-spec time-predicate)))
  (loop for i from init to (+ init range)
        when (satisfies-time-spec-p i time-spec)
          collect i))

(defun gen-unique-name (name)
  "Generate a unique name."
  (if (all-schedules-with-name name)
      (let ((new-name (format nil "~d/~a" *uuid* name)))
        (incf *uuid*)
        (gen-unique-name new-name))
      name))

(defun next-shots (schedule)
  "Return the times of the next few shots of SCHEDULE registered in
*ACTIONS*."
  (loop for time being the hash-keys of *actions*
        when (find schedule (gethash time *actions*))
          collect time))

(defmethod info ((schedule schedule))
  "Return the info of SCHEDULE."
  (let ((name (name schedule))
        (memo (memo schedule))
        (func (func schedule))
        (time (time-pred schedule))
        (enabled? (enabled schedule))
        (scheduled? (not (not (find schedule *schedules*))))
        (next-shots (next-shots schedule)))
    (alexandria:plist-hash-table
     (list :name       name
           :memo       memo
           :func       func
           :time       time
           :enabled?   enabled?
           :scheduled? scheduled?
           :next-shots next-shots))))
