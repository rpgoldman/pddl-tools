;;; -------------------------------------------------------------------------
;;; Copyright 2011-2016, SIFT, LLC, Robert P. Goldman, and Ugur Kuter
;;; Available under the BSD 3-clause license, see license.txt
;;;---------------------------------------------------------------------------

(defpackage :sift-pddl-utils-asd
  (:use :common-lisp :asdf))

(in-package :sift-pddl-utils-asd)

(defsystem :pddl-utils
    :name "SIFT-PDDL-UTILS"
    :license "BSD 3-clause (see license.txt)"
    :version "3.0"
    :depends-on (pddl iterate)
    :in-order-to ((test-op (test-op pddl-utils/tests)))
    :pathname "utils/"
    :components ((:file "package")      ; Package definition.
                 (:file "decls" :depends-on ("package"))
                 (:file "commons" :depends-on ("package" "decls"))
                 (:file "merger" :depends-on ("package"))
                 (:file "problem-macros" :depends-on ("package"))
;                 (:file "object-methods" :depends-on ("package" "decls"))
                 ))


(defsystem :pddl-utils/tests
  :depends-on (pddl-utils fiveam)
  :defsystem-depends-on (fiveam-asdf)
  :serial t
  :class :fiveam-tester-system
  :test-names ((#:problem-acc . :pddl-utils-tests)
               (#:domain-acc . :pddl-utils-tests)
               (#:domain-well-defined-p . :pddl-utils-tests)
               (#:predicate-definitions-correct-p . :pddl-utils-tests)
               (#:predicate-DONE-member-p . :pddl-utils-tests)
               (#:types-correct-p . :pddl-utils-tests)
               )
  :pathname "utils/tests/"
  :components ((:file "package")
               (:file "pddl-data")
               (:file "domain-test")))
