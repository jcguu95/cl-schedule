(in-package :cl-schedule)

(defparameter *dispatcher-name*
  "cl-schedule:global-dispatcher")

(defun wrap-with-simple-condition-handler (function &key extra-info)
  "Expect FUNCTION to take zero argument. Return the FUNCTION
wrapped with a condition handler."
  (lambda ()
    (handler-case (funcall function)
      (error (condition)
        (format t "
A function raises an unhandled condition:
  time       :: ~a
  function   :: ~a
  extra-info :: ~a
  condition  :: ~a
" (local-time:now) function extra-info condition)))))

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
                 (funcall (wrap-with-simple-condition-handler (func schedule)
                                                              :extra-info schedule)))
;;                (when (enabled schedule)
;;                  (let ((function (func schedule)))
;;                    (handler-case (funcall function)
;;                      (error (condition)
;;                        (format t "~%<CL-SCHEDULE> ~a
;; A schedule raises an unhandled condition:
;;   schedule: ~a
;;   condition: ~a
;; " (local-time:now) schedule condition)))))
            ))))

(defun reset-global-dispatcher ()
  (mapc #'bt:destroy-thread (global-dispatchers))
  (bt:make-thread #'global-dispatcher
                  :name *dispatcher-name*))

(reset-global-dispatcher)
