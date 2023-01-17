(in-package :cl-schedule-2)

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

(log:config :info)

(enable  (nth 2 *schedules*))
(disable (nth 2 *schedules*))
