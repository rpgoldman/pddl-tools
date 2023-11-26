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
    :version (:read-file-form "version.lisp-expr")
    :depends-on (hddl pddl-utils)
    :in-order-to ((test-op (test-op hddl-utils/tests)))
    :pathname "hddl-utils/"
    :components ((:file "package")      ; Package definition.
                 (:file "decls" :depends-on ("package"))
                 (:file "commons" :depends-on ("package" "decls"))
                 (:file "hddl-checker" :depends-on ("commons"))
                 ))


(defsystem :hddl-utils/tests
  :depends-on (pddl-utils fiveam)
  :defsystem-depends-on (fiveam-asdf)
  :version (:read-file-form "version.lisp-expr")
  :serial t
  :class :fiveam-tester-system
  :test-names ((#:hddl-tests . :hddl-utils-tests))
  :pathname "hddl-utils/tests/"
  :components ((:file "hddl-data")
               (:file "tests")))
