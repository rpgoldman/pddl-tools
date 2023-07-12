;;; -------------------------------------------------------------------------
;;; Copyright 2011-2016, SIFT, LLC, Robert P. Goldman, and Ugur Kuter
;;; Available under the BSD 3-clause license, see license.txt
;;;---------------------------------------------------------------------------

(defpackage :sift-hddl-asd
  (:use :common-lisp :asdf))

(in-package :sift-hddl-asd)

(defsystem :hddl
  :version "1.0"
  :license "BSD 3-clause (see license.txt)"
  :serial t
  :depends-on ((:version "pddl" "1.1") "cl-ppcre")
  :pathname "hddl/"
  :description "This system provides a package into which HDDL constructs
can be read, so that they can be shared across different programs manipulating
HDDL."
  :components ((:file "package")      ; Package definition.
               (:file "hddl-pprint") ; Pretty print and read functions
               ))
