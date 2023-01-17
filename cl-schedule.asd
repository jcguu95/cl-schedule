(defsystem :cl-schedule
  :name "cl-schedule"
  :description "cl-schedule is a cron-like scheduling library in
  common-lisp. It subsumes and replaces traditional cron managers
  thanks to richer expressiveness of Lisp."
  :author ("Gábor Melis <mega@retes.hu>"
           "Jin-Cheng Guu <jcguu95@gmail.com>")
  :license "MIT"
  :version "0.0.6"
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

;; TODO Name the old one by "v0", and this by "" (default).
(defsystem :cl-schedule-2
  :name "cl-schedule-2"
  :author ("Jin-Cheng Guu <jcguu95@gmail.com>")
  :license "MIT"
  :version "0.0.0"
  :components ((:module "src"
                :components ((:file "v2.package")
                             (:file "v2.setup")
                             (:file "v2.util")
                             (:file "v2.schedule")
                             (:file "v2.scheduler")
                             (:file "v2.dispatcher")
                             (:file "v2.main")
                             (:file "v2.test"))))
  :serial t
  :depends-on (:bordeaux-threads :local-time))
