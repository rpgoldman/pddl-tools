;;; -------------------------------------------------------------------------
;;; Copyright 2011-2016, SIFT, LLC, Robert P. Goldman, and Ugur Kuter
;;; Available under the BSD 3-clause license, see license.txt
;;;---------------------------------------------------------------------------

(defpackage :sift-pddl-planners-asd
  (:use :common-lisp :asdf))

(in-package :sift-pddl-planners-asd)

(defsystem :pddl-planners
    :name "SIFT-PDDL-PLANNERS"
    :license "BSD 3-clause (see license.txt)"
    :version (:read-file-form "version.lisp-expr")
    :depends-on (iterate pddl pddl-utils)
;;    :in-order-to ((test-op (test-op pddl-planners-tests)))
    :pathname "planners/"
    :components ((:file "package")      ; Package definition.
                 (:file "generics" :depends-on ("package"))
                 ))

#|
(defsystem :pddl-planners-tests
  :depends-on (pddl pddl-utils fiveam)
  :defsystem-depends-on (fiveam-asdf)
  :serial t
  :class :fiveam-tester-system
  ;; to be changed..
  :test-names ((#:problem-acc . :pddl-planners-tests)
               (#:domain-acc . :pddl-planners-tests)
               (#:domain-well-defined-p . :pddl-planners-tests))
  :pathname "planners/tests/"
  :components ((:file "package")
               (:file "pddl-data")
               (:file "planner-test")))
|#
