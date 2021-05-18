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
          (unless (%specification? specification)
            (error "SPECIFICATION must be a specification."))
          (cons specification step)))))

(defun %specification? (string)
  "A specification is a string that is either \"*\", \"n-m\", or
  a string satisfying the regex \"^[0-9*]+$\", where n and m are
  nonnegative integers. For example, \"1\", \"5-8\", \"1*\" are
  all specifications.

  This function returns whether STRING is a specification."
  ;; (specification? "3-9")
  ;; (specification? "3- 9")
  ;; (specification? "3-9.5")
  ;; (specification? "-3- 9")
  ;; (specification? "10- 9")
  (cond ((string= string "*")
         (values t "TODO give a good name"))
        ((when (cl-ppcre:all-matches "^[^-]*-[^-]*$" string)
           (let* ((splitted (cl-ppcre:split "-" string))
                  (a (car splitted))
                  (b (car (cdr splitted)))
                  (n (read-from-string a))
                  (m (read-from-string b)))
             (and (cl-ppcre:all-matches "^[0-9]*$" a)
                  (cl-ppcre:all-matches "^[0-9]*$" b)
                  (integerp n) (integerp m) (<= 0 n m))))
         (values t "TODO give a good name"))
        ((cl-ppcre:all-matches "^[0-9*]+$" string)
         (values t "TODO give a good name"))
        (t nil)))

;; TODO write a test:
;;
;; (loop for block in (mapcar #'block->terms (control-string->blocks "* 1,3,5-8/3 */7"))
;;                 collect (loop for term in block
;;                               collect (term->cons term)))
;; ;; => ((("*" . 1))
;;        (("1" . 1) ("3" . 1) ("5-8" . 3))
;;        (("*" . 7))
;;        (("*" . 1))
;;        (("*" . 1)))
