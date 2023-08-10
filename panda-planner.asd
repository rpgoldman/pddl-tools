;;;---------------------------------------------------------------------------
;;; Copyright 2023 Smart Information Flow Technologies, d/b/a SIFT, LLC
;;; and Robert P. Goldman
;;;
;;;---------------------------------------------------------------------------

(defpackage :panda-planner-asd
  (:use :common-lisp :asdf))

(in-package :panda-planner-asd)

(defsystem panda-planner
    :depends-on (pddl-planners)
  :version  (:read-file-form "version.lisp-expr")
  :pathname "planners/"
  :components ((:file "panda")))
