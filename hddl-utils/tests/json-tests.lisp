(in-package :common-lisp-user)

(defpackage hddl-json-tests
  (:use common-lisp iterate alexandria cl-json fiveam)
  (:import-from hddl-utils #:hddlify-tree)
  (:import-from hddl-json
                #:json-dump-domain
                #:json-dump-problem
                #:json-dump-atom
                #:json-dump-goal
                #:json-dump-quantified
                #:json-dump-task
                #:json-dump-htn))

(in-package :hddl-json-tests)

(def-suite* hddl-json-tests)

(test serialize-atoms
  (is (equalp "{\"predicate\":\"drive-ta\",\"args\":[\"?l1\",\"?l2\"]}"
              (with-output-to-string (json:*json-output*)
                (json-dump-atom '(SIFT-HDDL::DRIVE-TA SIFT-HDDL::?L1 SIFT-HDDL::?L2)))))
  (is (equalp "{\"predicate\":\"drive-ta\",\"args\":[]}"
              (with-output-to-string (json:*json-output*)
                (json-dump-atom '(SIFT-HDDL::DRIVE-TA))))))

(test serialize-tasks
    (is (equalp "{\"taskName\":\"drive-ta\",\"args\":[\"?l1\",\"?l2\"]}"
              (with-output-to-string (json:*json-output*)
                (json-dump-task '(SIFT-HDDL::DRIVE-TA SIFT-HDDL::?L1 SIFT-HDDL::?L2)))))
  (is (equalp "{\"taskName\":\"drive-ta\",\"args\":[]}"
              (with-output-to-string (json:*json-output*)
                (json-dump-task '(SIFT-HDDL::DRIVE-TA))))))

(test serialize-negations
  (is (equalp "{\"op\":\"not\",\"operand\":{\"predicate\":\"drive-ta\",\"args\":[\"?l1\",\"?l2\"]}}"
              (with-output-to-string (json:*json-output*)
                (hddl-json::json-dump-negation '(not (SIFT-HDDL::DRIVE-TA SIFT-HDDL::?L1 SIFT-HDDL::?L2))))))
  (is (equalp "{\"op\":\"not\",\"operand\":{\"predicate\":\"drive-ta\",\"args\":[\"?l1\",\"?l2\"]}}"
              (with-output-to-string (json:*json-output*)
                (hddl-json::json-dump-goal '(not (SIFT-HDDL::DRIVE-TA SIFT-HDDL::?L1 SIFT-HDDL::?L2))))))
  (is (equalp "{\"predicate\":\"drive-ta\",\"args\":[]}"
              (with-output-to-string (json:*json-output*)
                (hddl-json::json-dump-goal '(SIFT-HDDL::DRIVE-TA))))))

(test serialize-nary-goals
  (is (equalp "{\"op\":\"and\",\"operands\":[{\"predicate\":\"drive-ta\",\"args\":[\"?l1\",\"?l2\"]},{\"predicate\":\"pay-toll\",\"args\":[\"?l2\"]}]}"
              (with-output-to-string (str)
                (hddl-json::json-dump-nary
                 '(AND (SIFT-HDDL::DRIVE-TA SIFT-HDDL::?L1 SIFT-HDDL::?L2)
                   (SIFT-HDDL::PAY-TOLL SIFT-HDDL::?L2))
                 str))))
    (is (equalp "{\"op\":\"and\",\"operands\":[{\"predicate\":\"drive-ta\",\"args\":[\"?l1\",\"?l2\"]},{\"predicate\":\"pay-toll\",\"args\":[\"?l2\"]}]}"
              (with-output-to-string (str)
                (hddl-json::json-dump-goal
                 '(AND (SIFT-HDDL::DRIVE-TA SIFT-HDDL::?L1 SIFT-HDDL::?L2)
                   (SIFT-HDDL::PAY-TOLL SIFT-HDDL::?L2))
                 str))))
  (is (equalp "{\"op\":\"and\",\"operands\":[]}"
              (with-output-to-string (str)
                (hddl-json::json-dump-goal
                 '(AND)
                 str))))
    (is (equalp "{\"op\":\"or\",\"operands\":[{\"predicate\":\"drive-ta\",\"args\":[\"?l1\",\"?l2\"]},{\"predicate\":\"pay-toll\",\"args\":[\"?l2\"]}]}"
              (with-output-to-string (str)
                (hddl-json::json-dump-nary
                 '(OR (SIFT-HDDL::DRIVE-TA SIFT-HDDL::?L1 SIFT-HDDL::?L2)
                   (SIFT-HDDL::PAY-TOLL SIFT-HDDL::?L2))
                 str))))
    (is (equalp "{\"op\":\"or\",\"operands\":[{\"predicate\":\"drive-ta\",\"args\":[\"?l1\",\"?l2\"]},{\"predicate\":\"pay-toll\",\"args\":[\"?l2\"]}]}"
              (with-output-to-string (str)
                (hddl-json::json-dump-goal
                 '(OR (SIFT-HDDL::DRIVE-TA SIFT-HDDL::?L1 SIFT-HDDL::?L2)
                   (SIFT-HDDL::PAY-TOLL SIFT-HDDL::?L2))
                 str))))
  (is (equalp "{\"op\":\"or\",\"operands\":[]}"
              (with-output-to-string (str)
                (hddl-json::json-dump-goal
                 '(OR)
                 str)))))

(test serialize-quantifications
  (let ((prop (hddlify-tree '(forall (?pos - location) (not (mouse-at ?pos)))))
        (quant-string "{\"op\":\"forall\",\"boundVars\":[{\"name\":\"?pos\",\"type\":\"location\"}],\"operand\":{\"op\":\"not\",\"operand\":{\"predicate\":\"mouse-at\",\"args\":[\"?pos\"]}}}"))
    (is (equalp quant-string
                (with-output-to-string (str)
                  (json-dump-quantified prop str))))
        (is (equalp quant-string
                (with-output-to-string (str)
                  (json-dump-goal prop str))))
    (is (equalp
         "{\"name\":\"hunt_done\",\"parameters\":[],\"task\":{\"taskName\":\"hunt\",\"args\":[]},\"precondition\":{\"op\":\"forall\",\"boundVars\":[{\"name\":\"?pos\",\"type\":\"location\"}],\"operand\":{\"op\":\"not\",\"operand\":{\"predicate\":\"mouse-at\",\"args\":[\"?pos\"]}}},\"taskNetwork\":{\"orderedSubtasks\":[{\"taskName\":\"nil\",\"args\":[]}]}}"
         (with-output-to-string (str)
          (hddl-to-json::json-dump-method (hddlify-tree '(:method hunt_done
                                                          :parameters ()
                                                          :task (hunt)
                                                          :precondition (forall (?pos - location) (not (mouse-at ?pos)))
                                                          :ordered-subtasks ()))
                                          str))))))

(defun binary-present-p (binary &optional (error-p t))
  (multiple-value-bind (output error-output success)
      (uiop:run-program (list "/usr/bin/which" binary)
                        :ignore-error-status t
                        :output :string :error-output :output)
    (declare (ignore error-output))
    (or (zerop success)
        (null error-p)
        (progn (cerror "Install binary and retry" "Unable to find JSON manipulating binary \"~a\" in PATH.~%Output: ~a" binary output)
               t))))

(defvar *have-json-binaries* nil)

(defmacro with-json-binaries (&rest body)
  `(when (or *have-json-binaries*
             (and
              (binary-present-p "yq")
              (binary-present-p "check-jsonschema")))
     (setf *have-json-binaries* t)
     ,@body))

(defun yq-command ()
  (when (binary-present-p "yq")
    (let ((version-string (uiop:run-program (list "yq" "--version") :output :string))
          major-version-number)
      (multiple-value-bind (success matches)
          (cl-ppcre:scan-to-strings "yq +([^0-9]*)([0-9]+)" version-string)
        (unless success (error "Failed to extract version number from \"~a\"" version-string))
        (setf major-version-number (parse-integer (aref matches 1) :junk-allowed t))
        (ecase major-version-number
          (3 (list "yq" "."))
          (4 (list "yq" "--input-format" "yaml" "--output-format" "json"
                   "--exit-status")))))))

(defun yaml-to-json (input-file output-file)
  (let ((input-file (if (pathnamep input-file) (namestring input-file) input-file))
        (output-file (if (pathnamep output-file) (namestring output-file) output-file))
        (yq-command (yq-command)))
    (uiop:run-program (append yq-command (list input-file))
                                   :output output-file
                                   :error-output :string
                                   :ignore-error-status t)))


(test validate-domain
  (with-json-binaries
   (block test-block
     (let ((domain (hddl-utils:read-hddl-file
                    (namestring (asdf:system-relative-pathname "hddl-to-json" "hddl-utils/tests/ipc2020-syntactically-total-order-transport-domain.hddl"))))
           path-name-string)
       (uiop/driver:with-temporary-file (:stream str :pathname pn :prefix "transport-domain")
         (setf path-name-string (namestring pn))
         (is-true (typep domain 'hddl-utils::domain))
         (json-dump-domain domain str)
         :close-stream
         (uiop:with-temporary-file (:pathname schema-file :prefix "domain-schema")
           (let ((schema-filename (namestring schema-file)))
             (multiple-value-bind (ignore error-output success)
                  (yaml-to-json (namestring (asdf:system-relative-pathname "hddl-to-json" "json-schemas/domain.yaml"))
                                schema-file)
               (declare (ignore ignore))
               (is (zerop success))
               (unless (zerop success)
                 (format t "~&Exit code ~d for writing JSON schema in ~a~%. Error output: ~a~%"
                         success schema-filename error-output)
                 (return-from test-block nil)))
             (multiple-value-bind (output error-output success)
                 (uiop:run-program (list "check-jsonschema" "--schemafile" schema-filename path-name-string)
                                   :ignore-error-status t :error-output :output :output :string)
               (declare (ignore error-output))
               (is (zerop success))
               (unless (zerop success)
                 (format t "~&Exit code ~d for validating JSON against schema in ~a~%Error output:~%~a~%"
                         success schema-filename output)
                 (return-from test-block nil))))))))))

(test validate-problem
  (with-json-binaries
      (block test-block
        (let ((problem (hddl-utils:read-hddl-file
                        (namestring (asdf:system-relative-pathname "hddl-to-json" "hddl-utils/tests/ipc2020-syntactically-total-order-transport-p01.hddl"))))
              path-name-string)
          (uiop/driver:with-temporary-file (:stream str :pathname pn :prefix "transport-problem")
            (setf path-name-string (namestring pn))
            (is-true (typep problem 'hddl-utils:problem))
            (json-dump-problem problem str)
            :close-stream
            (uiop:with-temporary-file (:pathname schema-file :prefix "problem-schema")
              (let ((schema-filename (namestring schema-file)))
                (multiple-value-bind (ignore error-output success)
                    (yaml-to-json (namestring (asdf:system-relative-pathname "hddl-to-json" "json-schemas/problem.yaml"))
                                schema-file)
                  (declare (ignore ignore))
                  (unless (zerop success)
                    (format t "~&Exit code ~d for writing JSON schema in ~a~%. Error output: ~a~%"
                            success schema-filename error-output)
                    (return-from test-block nil))
                  (is (zerop success)))
                (multiple-value-bind (output error-output success)
                    (uiop:run-program (list "check-jsonschema" "--schemafile" schema-filename path-name-string)
                                      :ignore-error-status t :error-output :output :output :string)
                  (declare (ignore error-output))
                  (is (zerop success))
                  (unless (zerop success)
                    (format t "~&Exit code ~d for validating JSON against schema in ~a~%Error output:~%~a~%"
                            success schema-filename output)
                    (return-from test-block nil))))))))))

(test dump-task-net
  ;;; Test for problem processing a task network that's only a single subtask
  ;;; i.e., no leading AND.
  (let ((problem-htn '(:ORDERED-SUBTASKS (SIFT-HDDL::HUNT))))
    (is (equalp "{\"parameters\":[],\"orderedSubtasks\":[{\"taskName\":\"hunt\",\"args\":[]}]}"
                (with-output-to-string (str)
                  (json-dump-htn problem-htn str)))))
  (let ((method-def '(:METHOD SIFT-HDDL::MOVE-SHORT-SNAKE :PARAMETERS
                      (SIFT-HDDL::?SNAKE - SIFT-HDDL::SNAKE SIFT-HDDL::?SNAKEPOS
                       SIFT-HDDL::?GOALPOS SIFT-HDDL::?POS2 - SIFT-HDDL::LOCATION)
                      :TASK
                      (SIFT-HDDL::MOVE SIFT-HDDL::?SNAKE SIFT-HDDL::?SNAKEPOS
                       SIFT-HDDL::?GOALPOS)
                      :PRECONDITION
                      (AND (SIFT-HDDL::ADJACENT SIFT-HDDL::?POS2 SIFT-HDDL::?SNAKEPOS)
                       (NOT (SIFT-HDDL::OCCUPIED SIFT-HDDL::?POS2))
                       (SIFT-HDDL::TAIL SIFT-HDDL::?SNAKE SIFT-HDDL::?SNAKEPOS))
                      :ORDERED-SUBTASKS
                      (AND (SIFT-HDDL::MOVE-SHORT SIFT-HDDL::?SNAKE SIFT-HDDL::?POS2
                            SIFT-HDDL::?SNAKEPOS)
                       (SIFT-HDDL::MOVE SIFT-HDDL::?SNAKE SIFT-HDDL::?POS2
                        SIFT-HDDL::?GOALPOS)))))
    (is (equalp "{\"parameters\":[],\"orderedSubtasks\":[{\"taskName\":\"move-short\",\"args\":[\"?snake\",\"?pos2\",\"?snakepos\"]},{\"taskName\":\"move\",\"args\":[\"?snake\",\"?pos2\",\"?goalpos\"]}]}"
                (with-output-to-string (str)
                  (json-dump-htn (member :ordered-subtasks method-def) str))))))


