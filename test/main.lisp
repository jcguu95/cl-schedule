(defpackage :cl-schedule.test
  (:use #:common-lisp
        #:local-time
        #:cl-schedule)
  (:export))

(in-package :cl-schedule.test)

;;; dry-run

(assert
 (cl-schedule::equal-
  (mapcar #'local-time:universal-to-timestamp
          (dry-run '(:second 0
                     :minute 0
                     :hour   (8 20))
                   :init '(2020 1 1 12 59 59)
                   :range (* 3 86400)))
  '(@2020-01-01T20:00:00.000000+08:00
    @2020-01-02T08:00:00.000000+08:00
    @2020-01-02T20:00:00.000000+08:00
    @2020-01-03T08:00:00.000000+08:00
    @2020-01-03T20:00:00.000000+08:00
    @2020-01-04T08:00:00.000000+08:00)))

(defun fibonacci? (n)
  (find n '(0 1 2 3 5 8 13 21 34 55)))

(defparameter *a-complicated-time-spec*
  (lambda (time)
    (multiple-value-bind (second minute hour day month year)
        (decode-universal-time time)
      (and (= 0 second minute)
           (not (= 1 month))
           (fibonacci? (* (+ hour day) month))))))

(assert
 (cl-schedule::equal-
  (mapcar #'local-time:universal-to-timestamp
          (dry-run *a-complicated-time-spec*
                   :init  `(2020 10 10 12 35 59)
                   :range 10000000))
  `(@2020-11-01T04:00:00.000000+08:00
    @2020-11-02T03:00:00.000000+08:00
    @2020-11-03T02:00:00.000000+08:00
    @2020-11-04T01:00:00.000000+08:00
    @2020-11-05T00:00:00.000000+08:00
    @2021-02-01T00:00:00.000000+08:00
    @2021-02-01T03:00:00.000000+08:00
    @2021-02-01T16:00:00.000000+08:00
    @2021-02-02T02:00:00.000000+08:00
    @2021-02-02T15:00:00.000000+08:00
    @2021-02-03T01:00:00.000000+08:00)))

;;; satisfies-time-spec-p

(let ((utime (encode-universal-time 0 1 2 3 4 2000)))
  (assert
   (and (satisfies-time-spec-p utime #'evenp)
        (satisfies-time-spec-p utime '(:second 0))
        (satisfies-time-spec-p utime '(:second 0
                                       :minute 1))
        (satisfies-time-spec-p utime '(:second 0
                                       :minute 1
                                       :hour   2))
        (satisfies-time-spec-p utime '(:second 0
                                       :minute 1
                                       :hour   2
                                       :date   3
                                       :month  4
                                       :year   2000))
        (satisfies-time-spec-p utime `(:second (0 1 2) ; a list of integers
                                       :minute (< 0 _ 2) ; a list with at least one _
                                       :hour   (satisfies evenp) ; CL type specifier
                                       :day    () ; NIL means no restrictions
                                       :month  4  ; an integer
                                       :year   ,(lambda (x) (> x 1000)) ; a function
                                       )))))

;;; second%, minute%, hour%, date%, month%, year%, day%

(let ((utime (encode-universal-time 0 1 2 3 4 2000)))
  (assert
   (and (= (cl-schedule::second% utime) 0)
        (= (cl-schedule::minute% utime) 1)
        (= (cl-schedule::hour% utime) 2)
        (= (cl-schedule::date% utime) 3)
        (= (cl-schedule::month% utime) 4)
        (= (cl-schedule::year% utime) 2000)
        (= (cl-schedule::day% utime) 0))))

(flet ((yes (bool) (not (not bool))))
  (assert
   (and (yes (satisfies-generalized-type-p 0 nil))
        (yes (satisfies-generalized-type-p 0 0))
        (yes (satisfies-generalized-type-p 0 '(0 1 2)))
        (not (satisfies-generalized-type-p 0 '(1 2 3)))
        (yes (satisfies-generalized-type-p 0 '(member 0 1)))
        (not (satisfies-generalized-type-p 0 '(satisfies oddp)))
        (yes (satisfies-generalized-type-p 0 '(satisfies evenp)))
        (yes (satisfies-generalized-type-p 0 '(> 2 _)))
        (not (satisfies-generalized-type-p 0 '(< 2 _))))))

