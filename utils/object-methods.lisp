;;; -------------------------------------------------------------------------
;;; Copyright 2011-2016, SIFT, LLC, Robert P. Goldman, and Ugur Kuter
;;; Available under the BSD 3-clause license, see license.txt
;;;---------------------------------------------------------------------------

(in-package :pddl-utils)

(defmethod initialize-domain (domain-name domain-reqs)
  (make-instance 'pddl-domain
    :domain-name domain-name
    :requirements domain-reqs))

(defmethod initialize-problem (problem-name domain-name)
  (make-instance 'pddl-problem
    :problem-name problem-name
    :domain-name domain-name))

(defun problem->object (problem-expr)
  "Translate a PDDL problem s-expression into a PROBLEM object."
  (let* ((problem-name (problem-name problem-expr))
         (domain-name (problem-domain problem-expr))
         (problem (initialize-problem problem-name domain-name)))

    (setf (init-state problem) (problem-state problem-expr)
          (goal problem) (problem-goal problem-expr)
          (objects problem) (problem-objects problem-expr))
    problem))

(defun domain->object (domain-expr)
  "Translate a PDDL domain s-expression into a DOMAIN object."
  (let* ((domain-name (domain-name domain-expr))
         (reqs (sort (copy-list (domain-reqs domain-expr))
                     #'string<))
         (domain (initialize-domain domain-name reqs)))
    (setf (types domain) (domain-types domain-expr)
          (predicates domain) (domain-predicates domain-expr)
          (actions domain) (domain-actions domain-expr))
    domain))
