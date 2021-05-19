(in-package :cl-schedule)

;; This file aims to translate cron control strings into lispy
;; expressions.
;;
;; Example
;;
;; > "* 1,3,5-8/3 */7"
;; >        |
;; >        | #'control-string->blocks
;; >        v
;; > ("*" "1,3,5-8/3" "*/7" "*" "*")
;; >        |
;; >        | #'block->terms
;; >        v
;; > (("*") ("1" "3" "5-8/3") ("*/7") ("*") ("*"))
;; >        |
;; >        | #'term->cons
;; >        v
;; > ((("*" . 1))
;; >  (("1" . 1) ("3" . 1) ("5-8" . 3))
;; >  (("*" . 7))
;; >  (("*" . 1))
;; >  (("*" . 1)))
;;
;; We call for example "5-8/3" a term, which is transformed into
;; a cons ("5-8" . 3) of two atoms "5-8" and 3, in which "5-8" is
;; called the specification and 3 is called the step of the term
;; "5-8/3". The specification must be either "*", "n-m" where n <
;; m are nonnegative integers, or "+[0-9*]". The step must be
;; positive integer; it is set to 1 if unspecified.
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
;;
;; And it should turn ("5-12" . 3) to something like (5 8 11).
;; And it should turn ("*" . 3) to something like (0 3 6 9 12 15 18 21 24 .. 60).
;; And it should turn ("*7" . 3) to something like (7 37).

;; TODO Write tests.

(ql:quickload :cl-ppcre)                ;; FIXME remove this later.

(defun control-string->blocks (control-string)
  "A control string is a string with zero to five blocks. For
  example, the string \"* 1,3,5-8/3 */7\" has three blocks \"*\",
  \"1,3,5-8/3\", and \"*/7\". This function first test the input
  CONTROL-STRING against the rule, and extract the blocks from
  the CONTROL-STRING. If there are less than five blocks, then
  the result is completed by multiple \"*\"'s until it reaches
  five blocks. As an example,

  > input  \"* 1,3,5-8/3 */7\"
       |
       | #'control-string->blocks
       v
  > output (\"*\" \"1,3,5-8/3\" \"*/7\" \"*\" \"*\")"

  (let* ((result (cl-ppcre:split " " control-string))
         (length (length result)))
    (if (> length 5)
        (error "CONTROL-STRING must contain less than six blocks.")
        (append result (make-list (- 5 length) :initial-element "*")))))

(defun block->terms (block)
  "A block is a string without any whitespace. A term is a block
without any comma. This function returns the list of terms of the
given BLOCK. As an example,

  > input  \"1,3,5-8/3\"
       |
       | #'block->terms
       v
  > output (\"1\" \"3\" \"5-8/3\")"

  (cl-ppcre:split "," block))

;; TODO write test:
;;
;; (mapcar #'block->terms
;;         (control-string->blocks "* 1,3,5-8/3 */7"))
;;
;; ;; => (("*") ("1" "3" "5-8/3") ("*/7") ("*") ("*"))

(defun term->cons (term)
  "A term is a string that contains zero or one \"/\". A term
  without \"/\" should be thought as being post-appended by
  \"/1\". For example, the term \"*\" should be thought as
  \"*/1\".

  After such completion, the substring that comes before \"/\" is
  called the _specification_, while the substring that comes
  after \"/\" is called the _step_. For example, the
  specification and the step of the term \"5-8/3\" are \"5-8\"
  and \"3\" respectively. Both specification and step are called
  atoms. They must satisfy the following assumptions.

  First, the step must be a string of a positive integer. Second,
  the specification must be either \"*\", \"n-m\", or one
  satisfying the regex \"^[0-9*]+$\", where n < m are nonnegative
  integers. For example, \"1\", \"5-8\", \"1*\" are all
  specifications and thus terms. Hence \"1/7\", \"5-8/3\", and
  \"1*/5\" are also terms. On the other hand, \"3/4/5\" is not a
  term as it contains too many \"/\".

  This function parses and checks if TERM is a term. It then
  splits TERM into a cons consisting of the specification and
  step of TERM. If the step is NIL, it completes it by \"1\".
  Finally, it coerce the step into an integer. As an example,

  > input  \"5-8/3\"
       |
       | #'term->cons
       v
  > output (\"5-8\" . 3)"

  (let ((splitted (cl-ppcre:split "/" term)))
    (when (eql (cdr splitted) nil)
      (setf (cdr splitted) (list "1")))
    (if (not (<= 1 (length splitted) 2))
        (error "TERM must contain either one or two atoms.")
        (let ((specification (car splitted))
              (step (read-from-string (car (cdr splitted)))))
          (unless (and (integerp step) (> step 0))
            (error "STEP must be a positive integer."))
          (unless (specification-type specification)
            (error "SPECIFICATION must be a specification."))
          (cons specification step)))))

(defun specification-type (string)
  "A specification is a string that is either \"*\", \"n-m\", or
  a string satisfying the regex \"^[0-9*]+$\", where n and m are
  nonnegative integers. For example, \"1\", \"5-8\", \"1*\" are
  all specifications.

  This function returns the type of the STRING's specification as
  a generalized boolean, or NIL if STRING is not a
  specification."
  ;; TODO write a test for these
  ;; (specification-type "*")     ;; => 'all
  ;; (specification-type "3")     ;; => 'singleton
  ;; (specification-type "*3")    ;; => 'wildcard
  ;; (specification-type "3-9")   ;; => 'interval
  ;; (specification-type "9-3")   ;; => nil
  ;; (specification-type "3- 9")  ;; => nil
  ;; (specification-type "3-9.5") ;; => nil
  ;; (specification-type "-3- 9") ;; => nil
  ;; (specification-type "10- 9") ;; => nil
  (cond ((string= string "*") 'all)
        ((when (cl-ppcre:all-matches "^[^-]*-[^-]*$" string)
           (let* ((splitted (cl-ppcre:split "-" string))
                  (a (car splitted))
                  (b (car (cdr splitted)))
                  (n (read-from-string a))
                  (m (read-from-string b)))
             (and (cl-ppcre:all-matches "^[0-9]*$" a)
                  (cl-ppcre:all-matches "^[0-9]*$" b)
                  (integerp n) (integerp m) (<= 0 n m))))
         'interval)
        ((cl-ppcre:all-matches "^[0-9*]+$" string) 'wildcard)
        (t nil)))

;; TODO write a test:
;;
;; (loop for block in (control-string->blocks "* 1,3,5-8/3 */7")
;;       collect (loop for term in (block->terms block)
;;                     collect (term->cons term)))
;; ;; => ((("*" . 1))
;;        (("1" . 1) ("3" . 1) ("5-8" . 3))
;;        (("*" . 7))
;;        (("*" . 1))
;;        (("*" . 1)))

(defun make-integer-sequence
    (&optional (init 0) (step 1) (end 60) (regex "*"))
  ;; (make-integer-sequence 15 7 60 "[1-3].") ;; => (15 22 29 36)
  ;; (make-integer-sequence 15 7 60 "[13].")  ;; => (15 36)
  ;; (make-integer-sequence 15 2 60 "3*")     ;; => (31 33 35 37 39)
  (setf regex (concatenate 'string
                           "^"
                           (cl-ppcre:regex-replace-all
                            "\\*" regex ".*")
                           "$"))
  (remove-if-not
   (lambda (x) (cl-ppcre:all-matches
                regex (format nil "~a" x)))
   (when (<= init end)
     (cons init (make-integer-sequence
                 (+ init step) step end)))))

(defun cons->integer-sequence (cons)
  ;; (cons->integer-sequence (cons "3-20" 8)) ;; => (3 11 19)
  ;; (cons->integer-sequence (cons "*" 8))    ;; => (0 8 16 24 32 40 48 56)
  ;; (cons->integer-sequence (cons "*2" 6))   ;; => (12 42)
  (let* ((specification (car cons))
         (type (specification-type specification))
         (step (cdr cons))
         (n 0)
         (m 60))
    (when (eql type 'interval)
      (let* ((splitted (cl-ppcre:split "-" specification)))
        (setf n (read-from-string (car splitted)))
        (setf m (read-from-string (car (cdr splitted))))
        (setf specification "*")))
    (values (make-integer-sequence n step m specification)
            :n n :step step :m m :specification specification :type type)))

(defun control-string->lispy-result (control-string)
  "Main function of this file. It transforms a cron
control-string into a lispy expression."
  (let (result)
    (setf result (loop for block in (control-string->blocks control-string)
                       collect (loop for term in (block->terms block)
                                     collect (term->cons term))))
    (setf result
          (loop for x in '(0 1 2 3 4)
                collect (list (case x
                                (0 :minute)
                                (1 :hour)
                                (2 :day-of-month)
                                (3 :month)
                                (4 :day-of-week))
                              (let ((conss (nth x result)))
                                (if (eql 1 (length conss))
                                    (cons 'member (cons->integer-sequence (car conss)))
                                    (cons 'or (loop for cons in conss
                                                    collect (cons 'member (cons->integer-sequence cons))))))
                              )))
    (setf result (reduce #'append result))))

;; TODO write a test
;; (control-string->lispy-result "* 1,3,5-8/3 */7")
;; =>
;; (:MINUTE
;;  (MEMBER 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26
;;          27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49
;;          50 51 52 53 54 55 56 57 58 59 60)
;;
;;  :HOUR
;;  (OR (MEMBER 1) (MEMBER 3) (MEMBER 5 8))
;;
;;  :DAY-OF-MONTH
;;  (MEMBER 0 7 14 21 28 35 42 49 56)
;;
;;  :MONTH
;;  (MEMBER 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26
;;          27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49
;;          50 51 52 53 54 55 56 57 58 59 60)
;;
;;  :DAY-OF-WEEK
;;  (MEMBER 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26
;;          27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49
;;          50 51 52 53 54 55 56 57 58 59 60))
