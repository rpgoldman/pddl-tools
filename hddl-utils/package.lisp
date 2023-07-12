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
  ;; this one is different in HDDL as is the SETF method for domain actions...
  (:shadow #:insert-domain-actions #:domain-actions)
  (:reexport :pddl-utils :hddl-pprinter)
  (:import-from :pddl-utils
                #:action-sexp-p
                #:durative-action-sexp-p)
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
   #:remove-domain-tasks
   #:insert-domain-task
   #:insert-domain-tasks
   #:domain-methods
   #:remove-domain-methods
   #:insert-domain-method
   #:insert-domain-methods
   )
  #+nil(:export
   #:domain-merger
   #:problem-merger

   ;; move stuff into the pddl package
   #:pddlify
   #:pddlify-tree

   ;; generalized accessors
   #:problem-element
   #:domain-element

   ;; specific accessors
   #:domain-name
   #:problem-name
   #:problem-domain
   #:problem-objects
   #:problem-state
   #:problem-goal
   #:domain-predicates
   #:domain-reqs
   #:domain-types
   #:domain-functions
   #:domain-actions
   #:domain-constants
   #:action-precondition
   #:action-effects
   #:action-name
   #:action-params

   ;; translate s-expressions to objects
   #:domain->object
   #:problem->object

   ;; makers
   #:make-domain
   #:make-problem
   #:make-action
   #:defaction
   #:make-durative-action
   #:copy-domain
   #:copy-problem
   #:canonicalize-domain

   ;; miscellany
   #:remove-domain-actions
   #:insert-domain-actions
   #:exists-same
   #:add-to-domain-constants
   #:remove-types-from-list
   ;; simplifications
   #:canonicalize-types
   #:typelist-to-alist


   ;; I/O
   #:read-pddl-file
   #:pprint-pddl
   #:*pddl-pprint-dispatch-table*
   #:*pddl-package*
   #:print-pddl-plan-to-file
   #:print-pddl-plan
   #:read-pddl-plan
   #:read-pddl-plan-file

   ;; types
   #:action
   #:domain
   #:problem

   ;; constructor macros
   #:with-problem
   #:augmenting-problem
   ;; internal macros
   #:definstance
   #:facts
   ;; internal functions
   #:make-inst
   #:add-fact
   ))


