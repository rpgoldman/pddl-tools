;;; -------------------------------------------------------------------------
;;; Copyright 2011-2016, SIFT, LLC, Robert P. Goldman, and Ugur Kuter
;;; Available under the BSD 3-clause license, see license.txt
;;;---------------------------------------------------------------------------

(defpackage :sift-pddl-asd
  (:use :common-lisp :asdf))

(in-package :sift-pddl-asd)

(defsystem :pddl
  :version "1.0"
  :license "BSD 3-clause (see license.txt)"
  :serial t
  :pathname "pddl/"
  :description "This system provides a package into which PDDL constructs
can be read, so that they can be shared across different programs manipulating
PDDL.  Also provides a PDDL pretty-printer."
  :components ((:file "package")      ; Package definition.
               (:file "pddl-pprint") ; Pretty print functions
               ))
