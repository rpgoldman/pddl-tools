(in-package :common-lisp-user)

(defpackage :hddl-utils-tests
  (:use common-lisp hddl-utils fiveam)
  (:import-from :hddl-io #:partition-method-line
                #:complex-task-sexp-p))
