(in-package :cl)

(defpackage :cl-schedule
  (:use #:common-lisp
        #:local-time)
  (:export #:satisfies-time-spec-p
           #:schedule!
           #:unschedule!
           #:satisfies-generalized-type-p
           #:dry-run
           #:_))
