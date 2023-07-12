;;; -------------------------------------------------------------------------
;;; Copyright 2011-2016, SIFT, LLC, Robert P. Goldman, and Ugur Kuter
;;; Available under the BSD 3-clause license, see license.txt
;;;---------------------------------------------------------------------------

(defpackage :sift-hddl-utils-asd
  (:use :common-lisp :asdf))

(in-package :sift-hddl-utils-asd)

(defsystem :hddl-utils
    :name "SIFT-HDDL-UTILS"
    :license "BSD 3-clause (see license.txt)"
    :version "3.0"
    :depends-on (hddl pddl-utils)
    :in-order-to ((test-op (test-op hddl-utils/tests)))
    :pathname "hddl-utils/"
    :components ((:file "package")      ; Package definition.
                 (:file "decls" :depends-on ("package"))
                 #+nil(:file "commons" :depends-on ("package" "decls"))
                 #+nil(:file "merger" :depends-on ("package"))
                 #+nil(:file "problem-macros" :depends-on ("package"))
;                 (:file "object-methods" :depends-on ("package" "decls"))
                 ))


(defsystem :hddl-utils/tests
  :depends-on (pddl-utils fiveam)
  :defsystem-depends-on (fiveam-asdf)
  :serial t
  :class :fiveam-tester-system
  :test-names ((#:hddl-tests . :hddl-utils-tests)
               )
  :pathname "hddl-utils/tests/"
  :components ((:file "package")
               (:file "hddl-data")
               (:file "tests")))
