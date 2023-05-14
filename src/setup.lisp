(in-package :cl-schedule)
(declaim (optimize (debug 3) (safety 3)))

(local-time:enable-read-macros)

(defvar *schedules* nil)
(defvar *actions* (make-hash-table))   ; an action is a time and a schedule
(defvar *rounds* 30)              ; The scheduler schedules once every *ROUNDS* seconds.
(defparameter *counter* *rounds*)
(defparameter *uuid* 0)                 ; The uuid generator for generating unique name.
