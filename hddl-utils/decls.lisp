;;; -------------------------------------------------------------------------
;;; Copyright 2023, SIFT, LLC, Robert P. Goldman, and Ugur Kuter
;;; Available under the BSD 3-clause license, see license.txt
;;;---------------------------------------------------------------------------

(in-package :hddl-utils)

(defun hddl-interned (sym)
  (or (pddl-utils::pddl-interned sym)
      (eq (symbol-package sym) (find-package :hddl))))

(deftype hddl-symbol ()
  `(and symbol (satisfies hddl-interned)))

;; (defgeneric problem-htn (problem)
;;   (:documentation "Takes an HDDL PROBLEM thing and returns its htn,
;; a network of HTN tasks."))

;; (defgeneric (setf problem-htn) (htn problem))


#|
(defgeneric domain-name (domain)        ; ; ; ;
(:documentation "Takes a DOMAIN thing and returns its name, ; ; ; ;
which should be a symbol in the PDDL package.")) ; ; ; ;
                                        ; ; ; ;
(defgeneric (setf domain-name) (name domain)) ; ; ; ;
                                        ; ; ; ;
(defgeneric problem-name (problem)      ; ; ; ;
(:documentation "Takes a PROBLEM thing and returns its name, ; ; ; ;
which should be a symbol in the PDDL package.")) ; ; ; ;
                                        ; ; ; ;
(defgeneric (setf problem-name) (name problem)) ; ; ; ;
                                        ; ; ; ;
(defgeneric copy-domain (domain)        ; ; ; ;
(:documentation "Return a fresh copy of DOMAIN, that will ; ; ; ;
not share structure \(so can safely be destructively modified\).")) ; ; ; ;
                                        ; ; ; ;
(defgeneric copy-problem (problem)      ; ; ; ;
(:documentation "Return a fresh copy of PROBLEM, that will ; ; ; ;
not share structure \(so can safely be destructively modified\).")) ; ; ; ;
|#
