(in-package :common-lisp-user)

(defpackage hddl-json-tests
  (:use common-lisp iterate alexandria cl-json fiveam)
  (:import-from hddl-json
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
