(defsystem :clon
  :name "Clon"
  :author ("Gábor Melis <mega@retes.hu>"
           "Jin-Cheng Guu <jcguu95@gmail.com>")
  :version "0.0.2"
  :components ((:module "src"
                :components ((:file "package")
                             (:file "-clon")
                             (:file "clon"))))
  :serial t
  :depends-on (:bordeaux-threads :trivial-timers))

(defsystem :clon-test
  :depends-on (#:clon)
  :components ((:module "test"
                :components ((:file "package")
                             (:file "clon-test"))))
  :serial t)
