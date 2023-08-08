;;;---------------------------------------------------------------------
;;; Copyright (c) 2023 Smart Information Flow Technologies,
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
;;;    HDDL package definition
;;;
;;;---------------------------------------------------------------------

(in-package :common-lisp-user)

(uiop:define-package :sift-hddl
  (:nicknames :hddl)
  (:use #:pddl)
  (:documentation "API for HDDL manipulation based on PDDL-TOOLS.")
  (:reexport #:pddl))


(uiop:define-package hddl-io
  (:use :common-lisp :pddl-io :iterate)
  (:nicknames hddl-pprinter)
  (:reexport :pddl-io)
  (:import-from :pddl-io
                #:*pddl-pprint-dispatch*
                #:pprint-pddl
                #:read-pddl-file
                #:print-pddl-plan-to-file
                #:print-pddl-plan
                #:read-pddl-plan-file
                #:read-pddl-plan
                #:*canonical*
                #:*is-domain*
                #:*is-problem*
                #:domain-p
                #:problem-p
                #:toplevel-p
                #:typed-list-p
                #:pddl-var-p
                #:domain-spec-p
                #:define-keyword
                #:domain-keyword
                #:problem-keyword
                #:min-canonical-sublists 
                #:minimize-canonical-type-list 
                #:canonicalize-types
                )
  (:export #:pprint-hddl
           #:read-hddl-file
           #:read-hddl-plan
           #:read-hddl-plan-file
           #:hddl-symbol
           #:hddlify
           #:hddlify-tree
           #:*hddl-package*))

