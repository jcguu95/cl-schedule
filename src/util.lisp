(in-package :cl-schedule)

(defmethod equal- ((x null) (y null)) t)

(defmethod equal- ((x timestamp) (y timestamp))
  (local-time:timestamp= x y))

(defmethod equal- ((x cons) (y cons))
  (and (equal- (car x) (car y))
       (equal- (cdr x) (cdr y))))

(defun dry-run (time-spec
                &key
                  (init (get-universal-time))
                  (range 100))
  "Return the list of times (in universal format) from init
to (init+range) that satisfies TIME-SPEC."
  (when (consp init)
    (let ((year   (nth 0 init))
          (month  (nth 1 init))
          (date   (nth 2 init))
          (hour   (nth 3 init))
          (minute (nth 4 init))
          (second (nth 5 init)))
      (setf init (encode-universal-time second minute hour
                                        date   month  year))))
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
