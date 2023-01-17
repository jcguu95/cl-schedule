(in-package :cl-schedule-2)

(defun dry-run (time-predicate
                &key
                  (init (get-universal-time))
                  (range 100))
  "Return the list of times (in universal format) from init
to (init+range) that satisfies TIME-PREDICATE."
  (loop for i from init to (+ init range)
        when (funcall time-predicate i)
          collect i))
