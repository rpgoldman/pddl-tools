;;;---------------------------------------------------------------------
;;; Copyright (c) 2009-2016 Smart Information Flow Technologies,
;;; d/b/a SIFT, LLC.  Unpublished work.
;;;
;;; This software library is made publicly available under the BSD 3-clause
;;; license (see enclosed license.txt).
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
;;;    PDDL package definition
;;;
;;;---------------------------------------------------------------------

(in-package :common-lisp-user)

(defpackage :sift-pddl
  (:documentation "API for the PDDL reserved keywords for Murphy.")
  (:use)
  (:import-from :common-lisp
                #:and
                #:or
                #:not
                #:+
                #:-
                #:*
                #:/
                #:=
                #:<
                #:>
                #:<=
                #:>=
                #:when
                #:number)

  (:nicknames :pddl :sift.murphy-pddl :murphy-pddl)
  (:export
   #:*pddl-pprint-dispatch-table*
   #:pprint-pddl
   #:read-pddl-file
   #:read-pddl-plan-file
   #:read-pddl-plan
   #:print-pddl-plan-to-file
   #:print-pddl-plan

   ;; Domain/problem descriptions.
   #:define
   #:domain
   #:problem

   ;; I don't see these in the PDDL grammar...
;;   #:typed
;;   #:list
;;    #:function
;;   #:atomic
;;   #:formula
;;   #:skeleton

   ;; The special predicate we're using to designate the end point.
   #:goal

   ;; Not sure about these ones.
   ;; Aren't these all in the keyword package?
   #|
   #:init
   #:action
   #:requirements
   #:types
   #:predicates
   #:constants
   #:functions
   #:fluents
   #:durative-actions
   #:typing
   #:either ; this came up with discussion with Malte et al.
   #:precondition
   #:effect
   #:metric
   #:parallel #:length #:serial ;; ?


   ;; Are these really reserved keywords? Ah, I think they specify domain types:
   #:negative-preconditions
   #:disjunctive-preconditions
   #:existential-preconditions
   #:universal-preconditions
   #:quantified-preconditions
   #:conditional-effects
   #:equality
   #:strips
   #:adl
   #:duration-inequalities
   #:continuous-effects
   |#

   ;; Logical Expression in addition to AND, OR, NOT.
   #:imply
   #:exists
   #:forall

   ;; Keywords related to numeric fluents
   #:assign
   #:scale-up
   #:scale-down
   #:increase
   #:decrease
                                        ;   #:maximize
                                        ;   #:minimize
   #:total-time

   ;; Keywords related to temporal expressions
   #:at
   #:start
   #:end
   #:all
   #:over

   ;; passed through from COMMON-LISP
   #:and
   #:or
   #:not
   #:+
   #:-
   #:*
   #:/
   #:=
   #:<
   #:>
   #:<=
   #:>=
   #:when

   ;; built-in types
   #:object
   #:number

   ))

(defpackage pddl-io
  (:use :common-lisp :iterate)
  (:nicknames pddl-pprinter)
  (:export #:canonicalize-types
           #:pddl-symbol)
  (:import-from :pddl
                #:*pddl-pprint-dispatch-table*
                #:pprint-pddl
                #:read-pddl-file
                #:print-pddl-plan-to-file
                #:print-pddl-plan
                #:read-pddl-plan-file
                #:read-pddl-plan))

