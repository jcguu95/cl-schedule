(in-package :cl-schedule-2)

;; The main function in this file is #'satisfies-time-spec-p, which defines
;; the semantics of time-spec.

;; TODO Implement a function that compiles usual cron-style time
;; spec (e.g. "0 * * * *") into a time-spec.

(defun second% (&optional (universal-time (get-universal-time)))
  (multiple-value-bind (second minute hour date month year day)
      (decode-universal-time universal-time)
    (declare (ignore minute hour date month year day))
    second))

(defun minute% (&optional (universal-time (get-universal-time)))
  (multiple-value-bind (second minute hour date month year day)
      (decode-universal-time universal-time)
    (declare (ignore second hour date month year day))
    minute))

(defun hour% (&optional (universal-time (get-universal-time)))
  (multiple-value-bind (second minute hour date month year day)
      (decode-universal-time universal-time)
    (declare (ignore second minute date month year day))
    hour))

(defun date% (&optional (universal-time (get-universal-time)))
  (multiple-value-bind (second minute hour date month year day)
      (decode-universal-time universal-time)
    (declare (ignore second minute hour month year day))
    date))

(defun month% (&optional (universal-time (get-universal-time)))
  (multiple-value-bind (second minute hour date month year day)
      (decode-universal-time universal-time)
    (declare (ignore second minute hour date year day))
    month))

(defun year% (&optional (universal-time (get-universal-time)))
  (multiple-value-bind (second minute hour date month year day)
      (decode-universal-time universal-time)
    (declare (ignore second minute hour date month day))
    year))

(defun day% (&optional (universal-time (get-universal-time)))
  (multiple-value-bind (second minute hour date month year day)
      (decode-universal-time universal-time)
    (declare (ignore second minute hour date month year))
    day))

(let ((utime (encode-universal-time 0 1 2 3 4 2000)))
  (assert
   (and (= (second% utime) 0)
        (= (minute% utime) 1)
        (= (hour% utime) 2)
        (= (date% utime) 3)
        (= (month% utime) 4)
        (= (year% utime) 2000)
        (= (day% utime) 0))))

(defun satisfies-generalized-type-p (element generalized-type)
  "A generalized type is either NIL, a CL's type specification, an
integer, a list of integers, a function with one variable, or a
list with at least one symbol being _. This function returns
whether the ELEMENT is of the generalized type specified by
GENERALIZED-TYPE."
  (etypecase generalized-type
    (null     t)
    (integer  (= element generalized-type))
    (function (funcall generalized-type element))
    (list     (cond ((every #'integerp generalized-type)
                     (find element generalized-type))
                    ((find '_ generalized-type)
                     (eval (substitute element '_ generalized-type)))
                    (t
                     (typep element generalized-type))))))

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

(defun check-time-spec (time-spec)
  "A time-spec is either a function of one variable or a plist. If
it is a function, it must be a function with one variable, whose
value would be interpreted as a universal time. If it is a plist,
each of its keys must either be :second, :minute, :hour, :date,
:month, :year, or :day; and each of its values must be a
generalized-type."
  (or (functionp time-spec)
      (let ((keys (mapcar #'car (alexandria:plist-alist time-spec))))
        (setf keys (remove :second keys))
        (setf keys (remove :minute keys))
        (setf keys (remove :hour   keys))
        (setf keys (remove :date   keys))
        (setf keys (remove :month  keys))
        (setf keys (remove :year   keys))
        (setf keys (remove :day    keys))
        (unless (null keys)
          (error "TIME-SPEC contains unexpected keys.")))))

(defun satisfies-time-spec-p (universal-time time-spec)
  "This function returns whether UNIVERSAL-TIME
satisfies the TIME-SPEC."
  (check-time-spec time-spec)
  (etypecase time-spec
    (function (funcall time-spec universal-time))
    (list
     (and (satisfies-generalized-type-p (second% universal-time)
                                        (getf time-spec :second))
          (satisfies-generalized-type-p (minute% universal-time)
                                        (getf time-spec :minute))
          (satisfies-generalized-type-p (hour% universal-time)
                                        (getf time-spec :hour))
          (satisfies-generalized-type-p (date% universal-time)
                                        (getf time-spec :date))
          (satisfies-generalized-type-p (month% universal-time)
                                        (getf time-spec :month))
          (satisfies-generalized-type-p (year% universal-time)
                                        (getf time-spec :year))
          (satisfies-generalized-type-p (day% universal-time)
                                        (getf time-spec :day))))))

(let ((utime (encode-universal-time 0 1 2 3 4 2000)))
  (assert
   (and (satisfies-time-spec-p utime '(:second 0))
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
        (satisfies-time-spec-p utime `(:second (0 1 2)
                                       :minute (< 0 _ 2)
                                       :hour   (satisfies evenp)
                                       :day    ()
                                       :month  4
                                       :year   ,(lambda (x) (> x 1000)))))))
