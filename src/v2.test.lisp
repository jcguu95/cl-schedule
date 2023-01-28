(in-package :cl-schedule-2)

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

(mapcar #'local-time:universal-to-timestamp
        (dry-run `(:second (member 0 10 20 30 40 50))))

(mapcar #'local-time:universal-to-timestamp
        (dry-run '(:second (and (integer 2 25)
                            (satisfies fibonacci?))
                   :minute (satisfies mod5=>1))
                 :init  3882950000 ; universal time (@2023-01-17T05:13:20.000000-08:00)
                 :range 600        ; search range (seconds)
                 ))
;; =>
;; (@2023-01-17T05:16:02.000000-08:00 @2023-01-17T05:16:03.000000-08:00
;;  @2023-01-17T05:16:05.000000-08:00 @2023-01-17T05:16:08.000000-08:00
;;  @2023-01-17T05:16:13.000000-08:00 @2023-01-17T05:16:21.000000-08:00
;;  @2023-01-17T05:21:02.000000-08:00 @2023-01-17T05:21:03.000000-08:00
;;  @2023-01-17T05:21:05.000000-08:00 @2023-01-17T05:21:08.000000-08:00
;;  @2023-01-17T05:21:13.000000-08:00 @2023-01-17T05:21:21.000000-08:00)

;; TODO Use read-table of local-time and write test cases.

(mapcar #'local-time:universal-to-timestamp
        (dry-run (lambda (time)
                   (multiple-value-bind (second minute hour day month year)
                       (decode-universal-time time)
                     (and (= 0 second minute)
                          (not (= 1 month))
                          (fibonacci? (* (+ hour day) month)))))
                 :init  3882950000 ; universal time (@2023-01-17T05:13:20.000000-08:00)
                 :range 2000000    ; search range   (seconds)
                 ))
;; =>
;; (@2023-02-01T00:00:00.000000-08:00 @2023-02-01T03:00:00.000000-08:00
;;  @2023-02-01T16:00:00.000000-08:00 @2023-02-02T02:00:00.000000-08:00
;;  @2023-02-02T15:00:00.000000-08:00 @2023-02-03T01:00:00.000000-08:00
;;  @2023-02-03T14:00:00.000000-08:00 @2023-02-04T00:00:00.000000-08:00
;;  @2023-02-04T13:00:00.000000-08:00 @2023-02-05T12:00:00.000000-08:00
;;  @2023-02-06T11:00:00.000000-08:00 @2023-02-07T10:00:00.000000-08:00
;;  @2023-02-08T09:00:00.000000-08:00 @2023-02-09T08:00:00.000000-08:00)
