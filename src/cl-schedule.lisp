(in-package :cl-schedule)

;;;; Generic interface

(defparameter *default-next-time-limit*
  (encode-universal-time 0 0 0 1 1 3000)
  "The default time limit for NEXT-TIME searches.")

(defgeneric next-time (schedule &key init-time allow-now-p limit)
  (:documentation "Return the next time according to SCHEDULE or
NIL if there is no next time. If ALLOW-NOW-P the earliest
possible time to be returned is INIT-TIME, else it is usually
INIT-TIME + the resolution of the schedule. The default value of
INIT-TIME is (GET-UNIVERSAL-TIME), ALLOW-NOW-P is NIL and LIMIT
is *DEFAULT-NEXT-TIME-LIMIT*"))

(defun make-scheduler (schedule &key (init-time (get-universal-time))
                                  allow-now-p (limit *default-next-time-limit*))
  "Return a `scheduler' function of no arguments that returns times
from init-time on by repeatedly calling NEXT-TIME on SCHEDULE. ALLOW-NOW-P
is passed to the first invocation of NEXT-TIME."
  (lambda ()
    (prog1
        (setf init-time (next-time schedule :init-time init-time :allow-now-p allow-now-p
                                            :limit limit))
      (setf allow-now-p nil))))

(defun schedule-function (function scheduler
                          &key name (thread (bt:current-thread))
                            ignore-skipped immediate)
  "Create a timer just as with TRIVIAL-TIMERS:MAKE-TIMER but schedule
and reschedule FUNCTION according to SCHEDULER that is a function of
no parameters that returns a universal time or NIL. The returned timer
can be shut down with TRIVIAL-TIMERS:UNSCHEDULE-TIMER.

  If IMMEDIATE is NON-NIL, call the FUNCTION immediately after
this function is called. Otherwise, call the FUNCTION at the next
time given by the SCHEDULER.

If IGNORE-SKIPPED is NON-NIL, ignore the skipped jobs and emits
warnings accordingly. Otherwise, call the FUNCTION immediately
for each skipped instance. IGNORE-SKIPPED is by default set as
NIL for backward compatibility."
  (let (timer)
    (flet ((foo ()
             (let ((next-time (funcall scheduler))
                   (now (get-universal-time)))
               (when next-time
                 (trivial-timers:schedule-timer timer next-time
                                                :absolute-p t))
               (if ignore-skipped
                   (if (>= next-time now)
                       (if immediate
                           (funcall function)
                           (let ((delay-function-timer
                                   (trivial-timers:make-timer (lambda () (funcall function))
                                                              :name name
                                                              :thread thread)))
                             (trivial-timers:schedule-timer delay-function-timer
                                                            next-time
                                                            :absolute-p t)))
                       (warn "Action at time ~a skipped and omitted." next-time))
                   (funcall function)))))
      (setf timer
            (trivial-timers:make-timer #'foo
                                       :name name
                                       :thread thread)))
    (let ((first-time (funcall scheduler)))
      (when first-time
        (trivial-timers:schedule-timer timer first-time :absolute-p t)))
    timer))


;;;; Time utilities

;;; From sbcl's time.lisp
#-sbcl
(defun leap-years-before (year)
  (let ((years (- year 1901)))
    (+ (- (truncate years 4)
           (truncate years 100))
        (truncate (+ years 300) 400))))

(defun decode-universal-time* (time)
  "Return the decoded time components as a list instead of multiple
values."
  (multiple-value-list (decode-universal-time time)))

(defun encode-universal-time* (decoded-time)
  "Encode DECODED-TIME that is a decoded time in list form such as one
that was returned by DECODE-UNIVERSAL-TIME*."
  (apply #'encode-universal-time (subseq decoded-time 0 6)))

(defun days-of-month (decoded-time)
  "Return the number of days in the month DECODED-TIME."
  (let ((year (elt decoded-time 5))
        (month (elt decoded-time 4)))
    (+ (aref #(31 28 31 30 31 30 31 31 30 31 30 31) (1- month))
        (if (= 2 month)
            (- (leap-years-before (1+ year))
                (leap-years-before year))
            0))))

(defun min-valid-decoded-time-component (n)
  "Return the smallest valid value for the Nth decoded time component."
  (if (or (= n 3) (= n 4))
      1
      0))

(defun max-valid-decoded-time-component (n &optional decoded-time)
  "Return the largest valid value for the Nth component of
DECODED-TIME or NIL if there is no limit. Passing DECODED-TIME is
necessary only for the day of month component because the number of
days in a month varies."
  (ecase n
    ;; second and minute
    ((0 1) 59)
    ;; hour
    ((2) 23)
    ;; day of month
    ((3) (days-of-month decoded-time))
    ;; month
    ((4) 12)
    ;; year
    ((5) nil)
    ;; day of week
    ((6) 6)))

(defun valid-decoded-time-compenent-p (n value)
  "See if value can ever be a valid value as the Nth component of a
decoded time."
  (and (<= (min-valid-decoded-time-component n) value)
       (let ((limit (max-valid-decoded-time-component n '(0 0 0 1 1 2000))))
         (or (null limit) (<= value limit)))))

(defun zero-decoded-time-below (decoded-time n)
  "Set the first N components of DECODED-TIME to their
MIN-VALID-DECODED-TIME-COMPONENT values."
  (loop for i below n
        do (setf (elt decoded-time i)
                 (min-valid-decoded-time-component i))))

(defun bump-decoded-time (time n &optional (m 1))
  "Increment the Nth component of decoded TIME, handle overflows, zero
the lower components. Return changed decoded time and the highest index
that was changed."
  (let ((max n))
    (labels ((bump (time n)
               (setf max (max max n))
               (let ((limit (max-valid-decoded-time-component n time)))
                 (cond ((or (null limit) (< (elt time n) limit))
                        (incf (elt time n)))
                       (t
                        (setf (elt time n)
                              (min-valid-decoded-time-component n))
                        (bump time (1+ n)))))))
      (loop repeat m do (bump time n))
      (zero-decoded-time-below time max)
      (values time max))))


;;;; Cron-like schedule

(defun next-bump (bumper decoded-time n)
  "Invoke BUMPER on the Nth component of DECODED-TIME. Return its
value if it can ever be valid (not necessarily in the context of
DECODED-TIME) or else NIL."
  (let* ((original (elt decoded-time n))
         (next (cond ((null bumper) original)
                     ((eql bumper '*) original)
                     ((numberp bumper) bumper)
                     (t (funcall bumper original decoded-time n)))))
    (when (and next (not (valid-decoded-time-compenent-p n next)))
      (error "Invalid value ~S for decoded time component ~S." next n))
    (if (or (null next)
            (< next original)
            (and (= n 3)
                 (< (days-of-month decoded-time) next)))
        nil
        next)))

(defun bump-day-of-month-and-day-of-week (dom-bumper dow-bumper decoded-time)
  "Extra hair due to the circular dependency between DAY-OF-MONTH and
DAY-OF-WEEK bumpers. This function rolls the two bumpers into one."
  ;; First let DOM-BUMPER decide what's NEXT. If it is NIL we're done.
  ;; Else ask DOW-BUMPER what it thinks. If he likes it (i.e. returns
  ;; the same day of week) then NEXT is the result. Else skip to the
  ;; day of week it said or next Monday for NIL if it is still in the
  ;; same month. Repeat.
  ;;
  ;; To respect the convention of #'decode-universal-time, (0 1 2
  ;; 3 4 5 6) means (Mon Tue Wed Thu Fri Sat Sun). In particular
  ;; means (Mon Tue Wed).
  (let* ((decoded-time (copy-list decoded-time))
         (last-day-of-month (days-of-month decoded-time)))
    (flet ((skip-days (n)
             (incf (elt decoded-time 3) n)
             ;; Keep DECODED-TIME consistent, update DAY-OF-WEEK.
             (setf (elt decoded-time 6) (mod (+ (elt decoded-time 6) n) 7))))
      (loop while (<= (elt decoded-time 3) last-day-of-month) do
        (let* ((current-dom (elt decoded-time 3))
               (next-dom (next-bump dom-bumper decoded-time 3)))
          (unless next-dom
            (return nil))
          (skip-days (- next-dom current-dom))
          (assert (<= next-dom last-day-of-month)))
        (let* ((current-dow (elt decoded-time 6))
               (next-dow (next-bump dow-bumper decoded-time 6)))
          ;; See if the dom is also a blessed dow.
          (when (eql next-dow current-dow)
            (return (elt decoded-time 3)))
          ;; Skip until the prescribed day or next Monday.
          (skip-days (- (or next-dow 7) current-dow)))))))

(defclass cron-schedule ()
  ((bumpers :initarg :bumpers :reader bumpers
            :documentation "The bumpers in decoded time component order."))
  (:documentation "A cron-like schedule. See MAKE-CRON-SCHEDULE for details."))

(defun make-cron-schedule (&key second minute hour day-of-month
                             month year day-of-week)
  "Construct a cron-like scheduler from the SECOND, MINUTE, etc
bumpers for components of decoded times (using the default time zone
for the time being). A bumper in its most generic from a function of
three arguments: the current value, the whole decoded time, and the
index of the current value in the decoded time. A bumper is expected
to return the smallest value that is valid for that component and not
less than the current value or NIL if it cannot find a valid value.
Returning a value that is smaller than the current one is the same as
returning NIL. A bumper can simply be a number which is equivalent to
\(CONSTANTLY NUMBER).

Bumpers are not allowed to depend on `lower' components of the decoded
time. The allowed dependency graph is this:

SECOND -> MINUTE -> HOUR -> (DAY-OF-MONTH <-> DAY-OF-WEEK) -> MONTH -> YEAR

That is, the SECOND bumper may look at the whole decoded time but the
MINUTE bumper may not look at seconds. DAY-OF-WEEK and DAY-OF-MONTH
may depend on each other to allow specifying the 'last Friday of the
month'.

The resolution of the schedule is defined implicitly by the lowest
bumper. NEXT-TIME always bumps the component of the decoded time that
belongs to the lowest bumper before trying to find mathces if its
LAST-TIME argument is not NIL. Of course, DAY-OF-WEEK is the odd one
out: it is the day-of-month component that is bumped if DAY-OF-WEEK is
the lowest bumper.

This scheme allows (MAKE-CRON-SCHEDULE :MONTH 12) trigger only once
per year instead of every second in December. For a more packed
schedule one can use the symbol '* as a bumper: (MAKE-CRON-SCHEDULE
:HOUR '* :MONTH 12) which makes hour the lowest bumper and the
schedule triggers every hour in December.

It is an error if all bumpers are NIL."
  (let ((bumpers (list second minute hour day-of-month month year day-of-week)))
    (unless (some #'identity bumpers)
      (error "Cannot determine resolution of an empty schedule."))
    (make-instance 'cron-schedule :bumpers bumpers)))

(defun bumper-index->component-index (i)
  "Return the index of the decoded time component that is effect by
bumper I. Day of week is lumped together with day of month."
  (if (= i 6) 3 i))

(defun lowest-component-index-with-a-bumper (bumpers)
  "Return the the index of what is basically the root of current
dependency graph."
  (loop for i upfrom 0
        for bumper in bumpers
        when bumper
          minimize (bumper-index->component-index i)))

(defun bump-lowest-component (bumpers time)
  "Bump the lowest component of decoded TIME that has a bumper. Return
it as a universal time."
  (let ((decoded-time (decode-universal-time* time)))
    (encode-universal-time*
     (bump-decoded-time decoded-time
                        (lowest-component-index-with-a-bumper
                         bumpers)))))

(defmethod next-time ((schedule cron-schedule) &key (init-time (get-universal-time))
                                                 allow-now-p (limit *default-next-time-limit*))
  (let ((bumpers (bumpers schedule)))
    (unless allow-now-p
      (setf init-time (bump-lowest-component bumpers init-time)))
    (loop while (< init-time limit)
          with n = 5
          for decoded-time = (decode-universal-time* init-time)
          for next = (if (= n 3)
                         (bump-day-of-month-and-day-of-week (elt bumpers 3)
                                                            (elt bumpers 6)
                                                            decoded-time)
                         (next-bump (elt bumpers n) decoded-time n))
          do
             (cond ((null next)
                    (when (= n 5)
                      ;; The desired year is in the past, there is no next
                      ;; time.
                      (return nil))
                    ;; No valid value for this component, bump the next one
                    ;; and come again.
                    (multiple-value-setq (decoded-time n)
                      (bump-decoded-time decoded-time (1+ n))))
                   (t
                    (when (< (elt decoded-time n) next)
                      (setf (elt decoded-time n) next)
                      (zero-decoded-time-below decoded-time n))
                    (decf n)))
             (setf init-time (encode-universal-time* decoded-time))
             (when (minusp n)
               (return init-time)))))


;;;; The convenience case: typed cron schedule

(defun find-decoded-time-component-by-type (type value decoded-time n)
  "Return the first valid value not less than VALUE that is of TYPE."
  (loop with limit = (max-valid-decoded-time-component n decoded-time)
        for x upfrom value upto limit
        do (when (typep x type)
             (return-from find-decoded-time-component-by-type x))))

(defun make-typed-cron-bumper (type)
  "Return a bumper function suitable for MAKE-CRON-SCHEDULE that
returns the first valid value according to TYPE. Convenience function
on top of FIND-DECODED-TIME-COMPONENT-BY-TYPE."
  (lambda (value decoded-time n)
    (find-decoded-time-component-by-type type value decoded-time n)))

(defun make-typed-cron-schedule (&key second minute hour day-of-month
                                   month year day-of-week)
  "A convenience function much like MAKE-CRON-SCHEDULE but assumes
that no bumper can be a function designator so it must be a number,
the symbol * or a type specifier in which case it calls
MAKE-TYPED-CRON-BUMPER on it providing a terser syntax."
  (flet ((convert-bumper (bumper)
           (if (or (null bumper)
                   (typep bumper 'number)
                   (eq bumper '*))
               bumper
               (make-typed-cron-bumper bumper))))
    (let ((bumpers (list second minute hour day-of-month month year
                         day-of-week)))
      (make-instance 'cron-schedule
                     :bumpers (mapcar #'convert-bumper bumpers)))))
