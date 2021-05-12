(in-package :clon)

(defun new/schedule-function (function scheduler
                               &key name (thread (bt:current-thread))
                                 ignore-skipped immediate)
  "A newer schedule-function that supports to actions omissions
and warnings emissions for those jobs that are skipped. It is
designed to replace the old function, #'schedule-function. This
replacement will take place after being test for a while. Please
see its docstring as a reference.

If IMMEDIATE is NON-NIL, call the FUNCTION immediately after this
function is called. Otherwise, call the FUNCTION at the next time
given by the SCHEDULER.

If IGNORE-SKIPPED is NON-NIL, ignore the skipped jobs and emits
warnings accordingly. Otherwise, call the FUNCTION immediately
for each skipped instance. IGNORE-SKIPPED is by default set as
NIL for backward compatibility."
  (let (timer)
    (flet ((foo ()
             (let ((next-time (funcall scheduler))
                   (now (get-universal-time)))
               (when next-time
                 (trivial-timers:schedule-timer timer next-time
                                                :absolute-p t))
               (if ignore-skipped
                   (if (>= next-time now)
                       (if immediate
                           (funcall function)
                           (let ((delay-function-timer
                                   (trivial-timers:make-timer (lambda () (funcall function))
                                                              :name name
                                                              :thread thread)))
                             (trivial-timers:schedule-timer delay-function-timer
                                                            next-time
                                                            :absolute-p t)))
                       (warn "Action at time ~a skipped and omitted." next-time))
                   (funcall function)))))
      (setf timer
            (trivial-timers:make-timer #'foo
                                       :name name
                                       :thread thread)))
    (let ((first-time (funcall scheduler)))
      (when first-time
        (trivial-timers:schedule-timer timer first-time :absolute-p t)))
    timer))
