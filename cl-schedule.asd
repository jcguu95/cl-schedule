(defsystem :cl-schedule
  :name "cl-schedule"
  :description "cl-schedule is a cron-like scheduling library in
  common-lisp. It subsumes and replaces traditional cron managers
  thanks to richer expressiveness of Lisp."
  :author ("Gábor Melis <mega@retes.hu>"
           "Jin-Cheng Guu <jcguu95@gmail.com>")
  :license "MIT"
  :version "1.0.0"
  :components ((:module "src"
                :components ((:file "package")
                             (:file "user")
                             (:file "cl-schedule"))))
  :serial t
  :depends-on (:bordeaux-threads :trivial-timers :local-time))

(defsystem :cl-schedule-test
  :depends-on (#:cl-schedule)
  :components ((:module "test"
                :components ((:file "package")
                             (:file "cl-schedule-test"))))
  :serial t)
