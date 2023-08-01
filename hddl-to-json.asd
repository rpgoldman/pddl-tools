;;; -------------------------------------------------------------------------
;;; Copyright 2023, Robert P. Goldman and SIFT, LLC
;;; Available under the BSD 3-clause license, see license.txt
;;;---------------------------------------------------------------------------

(defpackage :sift-hddl-to-json-asd
  (:use :common-lisp :asdf))

(in-package :sift-hddl-to-json-asd)

(load-system :fiveam-asdf)

(defsystem :hddl-to-json
    :name "SIFT-HDDL-TO-JSON"
    :license "BSD 3-clause (see license.txt)"
    :version "3.0"
    :depends-on (hddl-utils cl-json iterate)
    :in-order-to ((test-op (test-op hddl-to-json/tests)))
    :pathname "hddl-utils/"
    :components ((:file "json")))

(defsystem :hddl-to-json/tests
  :depends-on (hddl-to-json fiveam)
  :defsystem-depends-on (fiveam-asdf)
  :serial t
  :class :fiveam-tester-system
  :test-names ((#:hddl-json-tests . :hddl-json-tests)
               )
  :pathname "hddl-utils/tests/"
  :components ((:file "json-tests")))