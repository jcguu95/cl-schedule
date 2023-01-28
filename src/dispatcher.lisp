(in-package :cl-schedule-2)

(defparameter *dispatcher-name*
  "cl-schedule:global-dispatcher")

(defun global-dispatchers ()
  "Return the lists of global dispatchers. We identify them by
superficially by thread names."
  (remove-if-not
   (lambda (thread)
     (string= *dispatcher-name* (bt:thread-name thread)))
   (bt:all-threads)))

(defun global-dispatcher ()
  (loop
    (sleep 0.5)
    (let ((now (get-universal-time)))
      (log:debug "Dispatching..")
      (loop for schedule in (gethash now *actions*)
            do (pop (gethash now *actions*))
               (when (enabled schedule)
                 (funcall (func schedule)))))))


(defun reset-global-dispatcher ()
  (mapc #'bt:destroy-thread (global-dispatchers))
  (bt:make-thread #'global-dispatcher
                  :name *dispatcher-name*))

(reset-global-dispatcher)
