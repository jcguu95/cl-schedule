(in-package :cl-schedule)

(log:config :info)

(schedule! :time (lambda (time)
                   (= 0 (mod (decode-universal-time time) 1)))
           :func (lambda () (log:info "Schedule-1 dispatched!"))
           :name "TEST-1"
           :memo "Just a test")

(schedule! :time (lambda (time)
                   (= 0 (mod (decode-universal-time time) 5)))
           :func (lambda () (log:info "Schedule-5 dispatched!"))
           :name "TEST-5"
           :memo "Just a test")

(schedule! :time (lambda (time)
                   (= 0 (mod (decode-universal-time time) 10)))
           :func (lambda () (log:info "Schedule-10 dispatched!"))
           :name "TEST-10"
           :memo "Just a test")

(disable-all-schedules)
(remove-all-schedules)

;; TODO Make this work: (typep 1 (is 1))
;; FIXME This doesn't seem to work!
(schedule! :time '(:second (member 0 10 20 30 40 50))
           :func (lambda () (log:info "Hello!"))
           :name "Greeter"
           :memo "Greets every 10 seconds.")

(defun mod5=>1 (n)
  (= 1 (mod n 5)))

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
 (equal- (mapcar #'local-time:universal-to-timestamp
                 (dry-run '(:second (0 10 20 30 40 50))
                          :init '(2020 1 1 12 59 59)
                          :range 100))
         `(@2020-01-01T13:00:00.000000+08:00
           @2020-01-01T13:00:10.000000+08:00
           @2020-01-01T13:00:20.000000+08:00
           @2020-01-01T13:00:30.000000+08:00
           @2020-01-01T13:00:40.000000+08:00
           @2020-01-01T13:00:50.000000+08:00
           @2020-01-01T13:01:00.000000+08:00
           @2020-01-01T13:01:10.000000+08:00
           @2020-01-01T13:01:20.000000+08:00
           @2020-01-01T13:01:30.000000+08:00)))

(assert
 (equal- (mapcar #'local-time:universal-to-timestamp
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

(assert
 (equal- (mapcar #'local-time:universal-to-timestamp
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
