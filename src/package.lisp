(in-package :cl)

(defpackage :cl-schedule
  (:use #:common-lisp)
  #+sbcl
  (:import-from :sb-impl #:leap-years-before)
  (:export
   ;; Useful macros for users
   #:schedule!
   #:unschedule
   #:all-schedules
   #:dry-run
   ;; Generic interface
   #:*default-next-time-limit*
   #:next-time
   #:make-scheduler
   #:schedule-function
   ;; Cron
   #:cron-schedule
   #:make-cron-schedule
   #:make-typed-cron-schedule
   #:find-decoded-time-component-by-type
   #:make-typed-cron-bumper))
