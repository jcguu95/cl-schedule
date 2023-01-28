(in-package :cl-schedule)

(defclass schedule ()
  ((func    :initarg :func
            :accessor func)
   (name    :initarg :name
            :accessor name)
   (memo    :initarg :memo
            :accessor memo)
   (time    :initarg :time
            :accessor time-pred)
   (enabled :initarg :enabled
            :initform t
            :accessor enabled)))

(defmethod print-object ((schedule schedule) stream)
  (format stream "#<schedule:~a>" (name schedule)))

(defmethod enable ((schedule schedule))
  ;; How about those that haven't been put into *schedules*?
  (setf (enabled schedule) t))
(defmethod disable ((schedule schedule))
  (setf (enabled schedule) nil))
(defmethod remove-schedule ((schedule schedule))
  (disable schedule)
  (setf *schedules* (remove schedule *schedules*)))

(defun enable-all-schedules ()
  (mapc #'enable *schedules*))
(defun disable-all-schedules ()
  (mapc #'disable *schedules*))
(defun remove-all-schedules ()
  (mapc #'remove-schedule *schedules*))
