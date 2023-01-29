(defsystem :cl-schedule.test
  :name "cl-schedule.test"
  :components ((:module "test"
                :components ((:file "main"))))
  :depends-on (:cl-schedule))
