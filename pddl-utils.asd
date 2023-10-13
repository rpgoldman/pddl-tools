;;; -------------------------------------------------------------------------
;;; Copyright 2011-2016,2023 SIFT, LLC, Robert P. Goldman, and Ugur Kuter
;;; Available under the BSD 3-clause license, see license.txt
;;;---------------------------------------------------------------------------

(defpackage :sift-pddl-utils-asd
  (:use :common-lisp :asdf))

(in-package :sift-pddl-utils-asd)

(defsystem :pddl-utils
    :name "SIFT-PDDL-UTILS"
    :license "BSD 3-clause (see license.txt)"
    :version (:read-file-form "version.lisp-expr")
    :depends-on (pddl iterate alexandria)
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
  :version (:read-file-form "version.lisp-expr")
  :serial t
  :class :fiveam-tester-system
  :test-names ((#:pddl-utils-tests . :pddl-utils-tests)
               )
  :pathname "utils/tests/"
  :components ((:file "package")
               (:file "pddl-data")
               (:file "domain-test")))
