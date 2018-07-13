;;;---------------------------------------------------------------------
;;; Copyright (c) 2009-2016 Smart Information Flow Technologies,
;;; d/b/a SIFT, LLC.
;;;
;;; This code made available according to the BSD 3-clause license (see
;;; license.txt)
;;;
;;; GOVERNMENT PURPOSE RIGHTS
;;;
;;; Contract No.         FA8650-06-C-7606,
;;; Contractor Name      Smart Information Flow Technologies, LLC
;;;                      d/b/a SIFT, LLC
;;; Contractor Address   211 N 1st Street, Suite 300
;;;                      Minneapolis, MN 55401
;;; Expiration Date      5/2/2011
;;;
;;; The Government's rights to use, modify, reproduce, release,
;;; perform, display, or disclose this software are restricted by
;;; paragraph (b)(2) of the Rights in Noncommercial Computer Software
;;; and Noncommercial Computer Software Documentation clause contained
;;; in the above identified contract. No restrictions apply after the
;;; expiration date shown above. Any reproduction of the software or
;;; portions thereof marked with this legend must also reproduce the
;;; markings.
;;;---------------------------------------------------------------------
;;; File Description:
;;;
;;;   SIFT's PDDL Planner Wrappers.
;;;
;;;---------------------------------------------------------------------

(in-package :common-lisp-user)

(defpackage :sift-pddl-planners
  (:documentation "Utilities for calling PDDL planners from LISP.")
  (:use common-lisp pddl-utils iterate)
  (:nicknames :pddl-planners)
;;;  (:import-from #:pddl
;;;                #:read-pddl-file
;;;                #:pprint-pddl
;;;                #:*pddl-pprint-dispatch-table*)

  (:export
   #:run-planner
   #:validate
   #:read-plan-file
   #:string-to-action
   ))
