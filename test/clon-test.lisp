(in-package :clon-test)

(defmacro assert-error (&body body)
  `(assert (nth-value 1 (ignore-errors (progn ,@body)))))

(defun test-next-time ()
  (let ((now '(15 30 3 23 5 1974)))
    (flet ((foo (spec result &key (init-time init-time) allow)
             (let ((r (next-time spec
                                 :allow-now-p allow
                                 :init-time (clon::encode-universal-time* now))))
               (assert (or (and (null r) (null result))
                           (equal result
                                  (subseq (clon::decode-universal-time* r)
                                          0 6)))))))
      (assert-error (foo (make-cron-schedule) '(15 30 3 23 5 1974)))
      (foo (make-cron-schedule :second 14) '(14 31 3 23 5 1974) :allow t)
      (foo (make-cron-schedule :second 15) '(15 30 3 23 5 1974) :allow t)
      (foo (make-cron-schedule :second 15) '(15 31 3 23 5 1974))
      (foo (make-cron-schedule :second 16) '(16 30 3 23 5 1974))
      (foo (make-cron-schedule :minute 29) '(0 29 4 23 5 1974) :allow t)
      (foo (make-cron-schedule :minute 30) '(15 30 3 23 5 1974) :allow t)
      (foo (make-cron-schedule :minute 30) '(0 30 4 23 5 1974))
      (foo (make-cron-schedule :minute 31) '(0 31 3 23 5 1974))
      (foo (make-cron-schedule :hour 2) '(0 0 2 24 5 1974) :allow t)
      (foo (make-cron-schedule :hour 3) '(15 30 3 23 5 1974)  :allow t)
      (foo (make-cron-schedule :hour 3) '(0 0 3 24 5 1974))
      (foo (make-cron-schedule :hour 4) '(0 0 4 23 5 1974))
      (foo (make-cron-schedule :day-of-month 22) '(0 0 0 22 6 1974) :allow t)
      (foo (make-cron-schedule :day-of-month 23) '(15 30 3 23 5 1974) :allow t)
      (foo (make-cron-schedule :day-of-month 23) '(0 0 0 23 6 1974))
      (foo (make-cron-schedule :day-of-month 24) '(0 0 0 24 5 1974))
      (foo (make-cron-schedule :month 4) '(0 0 0 1 4 1975) :allow t)
      (foo (make-cron-schedule :month 5) '(15 30 3 23 5 1974) :allow t)
      (foo (make-cron-schedule :month 5) '(0 0 0 1 5 1975))
      (foo (make-cron-schedule :month 6) '(0 0 0 1 6 1974))
      (foo (make-cron-schedule :year 1973) nil :allow t)
      (foo (make-cron-schedule :year 1974) '(15 30 3 23 5 1974) :allow t)
      (foo (make-cron-schedule :year 1974) nil)
      (foo (make-cron-schedule :year 1975) '(0 0 0 1 1 1975))
      (foo (make-cron-schedule :day-of-month 1 :month 5) '(0 0 0 1 5 1975))
      ;; Test limit
      (foo (make-cron-schedule :year 40000) nil)
      ;; Hour 3 -> hour 2 -> hour overflow -> day overflow -> month
      ;; overflow -> year overflow.
      (foo (make-cron-schedule :hour 2 :month 5) '(0 0 2 1 5 1975)
           :init-time '(15 30 3 31 5 1974))
      ;; Maximum overflow.
      (foo (make-cron-schedule :second 0) '(0 0 0 1 1 1975)
           :init-time '(1 59 23 31 12 1974))
      ;; Thursday -> Saturday
      (foo (make-cron-schedule :day-of-week 5) '(0 0 0 25 5 1974))
      ;; Thursday -> Wednesday
      (foo (make-cron-schedule :day-of-week 2) '(0 0 0 29 5 1974))
      ;; February doesn't have 31 days.
      (foo (make-cron-schedule :day-of-month 31) '(0 0 0 31 3 1974)
           :init-time '(15 30 3 20 2 1974))
      ;; So this never happens:
      (foo (make-cron-schedule :day-of-month 31 :month 2) nil)
      ;; Simple function bumpers.
      (foo (make-cron-schedule :second (constantly 15)) '(15 30 3 23 5 1974)
           :allow t)
      (foo (make-cron-schedule :second (constantly 19)) '(19 30 3 23 5 1974)
           :allow t)
      (foo (make-cron-schedule :second (constantly 10)) '(10 31 3 23 5 1974)
           :allow t)
      ;; Hairy typed bumpers.
      (let ((hairy (make-cron-schedule
                    :second (make-typed-cron-bumper '(member 0 15 30 45))
                    :minute (make-typed-cron-bumper '(and (integer 10 40)
                                                      (satisfies evenp))))))
        (foo hairy '(15 30 3 31 5 1974) :init-time '(15 30 3 31 5 1974) :allow t)
        (foo hairy '(30 30 3 31 5 1974) :init-time '(15 30 3 31 5 1974))
        (foo hairy '(15 30 3 31 5 1974) :init-time '(13 30 3 31 5 1974))
        (foo hairy '(0 32 3 31 5 1974) :init-time '(48 30 3 31 5 1974))))))

(defun test-clon ()
  (test-next-time))
