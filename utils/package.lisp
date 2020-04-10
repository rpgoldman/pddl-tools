;;;---------------------------------------------------------------------
;;; Copyright (c) 2009-2020 Smart Information Flow Technologies,
;;; d/b/a SIFT, LLC.
;;;
;;; This code made available according to the BSD 3-clause license (see
;;; license.txt)
;;;
;;;---------------------------------------------------------------------
;;; File Description:
;;;
;;;   SIFT's PDDL utilities.
;;;
;;;---------------------------------------------------------------------

(in-package :common-lisp-user)

(defpackage :sift-pddl-utils
  (:documentation "Utilities for wrangling PDDL domain and problem files.")
  (:use common-lisp pddl iterate)
  (:nicknames :pddl-utils)
  (:import-from #:pddl-pprinter
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
  (:export
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
   #:domain-action
   #:domain-constants

   ;;
   #:action-precondition
   #:action-effect
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


