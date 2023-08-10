(in-package :common-lisp-user)

(defpackage :pddl-utils-tests
  (:use common-lisp pddl-utils)
  (:import-from :pddl-utils #:flatten-conjunction)
  (:import-from fiveam
                #:def-fixture
                #:with-fixture
                #:is
                #:test
                #:run!
                #:*on-error*))
