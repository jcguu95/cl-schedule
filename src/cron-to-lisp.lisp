(in-package :cl-schedule)

;; This file aims to translate cron control strings into lispy
;; expressions.
;;
;; Example "* 1,3,5-8/3 */7":
;;
;; > ("*" "1,3,5-8/3" "*/7" "*" "*")
;; > (("*") ("1" "3" "5-8/3") ("*/7") ("*") ("*"))
;; > ((("*" . 1))
;;    (("1" . 1) ("3" . 1) ("5-8" . 3))
;;    (("*" . 7))
;;    (("*" . 1))
;;    (("*" . 1)))
;;
;; So far, we call for example ("5-8" . 3) a term, in which "5-8"
;; is called the specification and 3 is called the step.
;;
;; We will write a magic function that translates a term into a
;; lisp expression (ingredient: init, step, a filter function).
;;
;; The end result of the example above should be
;;
;; > (:minute       ("*" . 1)
;;    :hour         (or ("1"   . 1)
;;                      ("3"   . 1)
;;                      ("5-8" . 3))
;;    :day-in-month ("*" . 7)
;;    :month        ("*" . 1)
;;    :day-in-week  ("*" . 1))
