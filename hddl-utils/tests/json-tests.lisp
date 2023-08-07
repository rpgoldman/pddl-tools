(in-package :common-lisp-user)

(defpackage hddl-json-tests
  (:use common-lisp iterate alexandria cl-json fiveam)
  (:import-from hddl-json
                #:json-dump-domain
                #:json-dump-problem
                #:json-dump-atom
                #:json-dump-task))

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

(defun binary-present-p (binary)
  (multiple-value-bind (output error-output success)
      (uiop:run-program (list "/usr/bin/type" binary)
                        :ignore-error-status t
                        :output :string :error-output :output)
    (declare (ignore error-output))
    (or (zerop success)
        (progn (cerror "Install binary and retry" "Unable to find JSON manipulating binary \"~a\" in PATH.~%Output: ~a" binary output)
               t))))

(defmacro with-json-binaries (&rest body)
  `(when (and
          (binary-present-p "yq")
          (binary-present-p "check-jsonschema"))
     ,@body))

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
                 (uiop:run-program (list "yq" "--input-format" "yaml" "--output-format" "json"
                                         "--exit-status"
                                         (namestring (asdf:system-relative-pathname "hddl-to-json" "json-schemas/domain.yaml")))
                                   :output schema-file
                                   :error-output :string
                                   :ignore-error-status t)
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
                    (uiop:run-program (list "yq" "--input-format" "yaml" "--output-format" "json"
                                            "--exit-status"
                                            (namestring (asdf:system-relative-pathname "hddl-to-json" "json-schemas/problem.yaml")))
                                      :output schema-file
                                      :error-output :string
                                      :ignore-error-status t)
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
