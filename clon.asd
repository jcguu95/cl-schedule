;;;; -*- Mode: Lisp -*-

(cl:defpackage #:clon-system
  (:use :cl :asdf))

(cl:in-package #:clon-system)

(defsystem :clon
  :name "Clon"
  :author ("Gábor Melis <mega@retes.hu>"
           "Jin-Cheng Guu <jcguu95@gmail.com>")
  :version "0.0.2"
  :components ((:file "packages")
               (:file "-clon")
               (:file "clon"))
  :serial t
  :depends-on (:bordeaux-threads :trivial-timers))
