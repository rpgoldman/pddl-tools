;;; -------------------------------------------------------------------------
;;; Copyright 2011-2016, SIFT, LLC, Robert P. Goldman, and Ugur Kuter
;;; Available under the BSD 3-clause license, see license.txt
;;;---------------------------------------------------------------------------

(in-package :pddl-utils)

(defun pddl-interned (sym)
  (eq (symbol-package sym) (find-package :pddl)))

(deftype pddl-symbol ()
  `(and symbol (satisfies pddl-interned)))

;; The UKUTER-PDDL-UTILS package uses this PDDL-DOMAIN/PROBLEM class for
;; easy-access/manipulation to the elements of a PDDL domain/problem. Currently, the API to the
;; package is through s-expressions only.

(defgeneric domain-name (domain)
  (:documentation "Takes a DOMAIN thing and returns its name,
which should be a symbol in the PDDL package."))

(defgeneric (setf domain-name) (name domain))

(defgeneric problem-name (problem)
  (:documentation "Takes a PROBLEM thing and returns its name,
which should be a symbol in the PDDL package."))

(defgeneric (setf problem-name) (name problem))

(defgeneric copy-domain (domain)
  (:documentation "Return a fresh copy of DOMAIN, that will
not share structure \(so can safely be destructively modified\)."))

(defgeneric copy-problem (problem)
  (:documentation "Return a fresh copy of PROBLEM, that will
not share structure \(so can safely be destructively modified\)."))
