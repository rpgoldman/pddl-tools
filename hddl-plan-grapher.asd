;;; -------------------------------------------------------------------------
;;; Copyright 2024, SIFT, LLC, Robert P. Goldman, and Ugur Kuter
;;; Available under the BSD 3-clause license, see license.txt
;;;---------------------------------------------------------------------------

(defpackage :sift-hddl-plan-grapher-asd
  (:use :common-lisp :asdf))

(in-package :sift-hddl-plan-grapher-asd)

(defsystem :hddl-plan-grapher
    :name "SIFT-HDDL-UTILS"
    :license "BSD 3-clause (see license.txt)"
    :version (:read-file-form "version.lisp-expr")
    :depends-on (hddl-utils hddl pddl-utils cl-dot)
    ;; :in-order-to ((test-op (test-op hddl-utils/tests)))
    :pathname "hddl-plan-grapher/"
    :serial t
    :components ((:file "package")      ; Package definition.
                 (:file "decls")
                 (:file "plan-grapher")
                 ))

#|
(defsystem :hddl-plan-grapher/tests
  :depends-on (pddl-utils fiveam)
  :defsystem-depends-on (fiveam-asdf)
  :version (:read-file-form "version.lisp-expr")
  :serial t
  :class :fiveam-tester-system
  :test-names ((#:hddl-tests . :hddl-plan-grapher-tests))
  :pathname "hddl-plan-grapher/tests/"
  :components ((:file "hddl-data")
               (:file "tests")))
|#
