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
;;;   SIFT's PDDL utilities.
;;;
;;;---------------------------------------------------------------------

(in-package :common-lisp-user)

(uiop:define-package :sift-hddl-utils
  (:documentation "Utilities for wrangling PDDL domain and problem files.")
  (:use :common-lisp :hddl :iterate :pddl-utils :hddl-pprinter)
  (:nicknames :hddl-utils)
  (:shadow #:insert-domain-actions #:domain-actions
           #:domain-predicates
           #:make-domain #:make-problem
           #:canonicalize-domain
           #:problem-domain
           #:problem-goal
           #:problem-state)
  (:reexport :pddl-utils :hddl-pprinter)
  (:import-from :alexandria #:if-let)
  (:import-from :pddl-utils
                #:action-sexp-p
                #:has-element-p
                #:durative-action-sexp-p
                #:flatten-conjunction)
  #+nil (:import-from #:pddl-pprinter
                      #:complex-task-sexp-p
                      #:*pddl-package*
                      #:pddl-symbol
                      #:canonicalize-types
                      #:read-pddl-file
                      #:pprint-pddl
                      #:*pddl-pprint-dispatch-table*
                      #:print-pddl-plan-to-file
                      #:print-pddl-plan
                      #:read-pddl-plan-file
                      #:read-pddl-plan)
  (:import-from :hddl-io #:complex-task-sexp-p)
  (:export
   #:problem-htn
   #:domain-tasks
   #:domain-predicates
   #:remove-domain-tasks
   #:insert-domain-task
   #:insert-domain-tasks
   #:domain-methods
   #:remove-domain-methods
   #:insert-domain-method
   #:insert-domain-methods
   #:problem-goal
   #:ordered-method-p
   #:method-def ;type
   #:ordered-method-def                 ;type
   #:method-name
   #:method-task-net
   #:method-subtasks
   #:method-task
   #:method-parameters
   #:method-precondition
   #:task-def ; type
   #:task-name
   #:task-parameters
   #:p-htn ; type -- problem HTN
   #:p-htn-parameters
   #:p-htn-ordered-subtasks
   #:predicate-name
   #:predicate-parameters
   #:hddl-plan-to-pddl-plan
   #:hddl-domain-to-pddl-domain
   #:hddl-problem-to-pddl-problem
   #:canonicalize-problem
   #:hddl-variable
   )
)
