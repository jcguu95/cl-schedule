(defsystem :cl-schedule
  :name "cl-schedule"
  :author ("Jin-Cheng Guu <jcguu95@gmail.com>")
  :license "MIT"
  :version "2.0.0"
  :components ((:module "src"
                :components ((:file "package")
                             (:file "setup")
                             (:file "util")
                             (:file "schedule")
                             (:file "scheduler")
                             (:file "dispatcher")
                             (:file "main")
                             (:file "test"))))
  :serial t
  :depends-on (:bordeaux-threads :local-time))
