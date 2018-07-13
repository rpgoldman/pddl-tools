;;; -------------------------------------------------------------------------
;;; Copyright 2011-2016, SIFT, LLC, Robert P. Goldman, and Ugur Kuter
;;; Available under the BSD 3-clause license, see license.txt
;;;---------------------------------------------------------------------------

(in-package :pddl-utils)

;; Below, the API inputs are s-expressions.
(defun domain-merger (pddl-expr1 pddl-expr2)
  (let ((domain1 pddl-expr1)
        (domain2 pddl-expr2))
    ;; The two expressions should describe parts of the same domain.
    (unless (or (eq (domain-name domain1) (domain-name domain2))
                (null (domain-name domain1))
                (null (domain-name domain2)))
      (error "Domains to be merged must be consistent in name.  ~A and ~A disagree."
             (domain-name domain1)
             (domain-name domain2)))
    (unless (or (equal (domain-reqs domain1) (domain-reqs domain2))
                (null (domain-reqs domain1))
                (null (domain-reqs domain2)))
      (error "Domains must agree on requirements.  ~A and ~A clash."
             (domain-reqs domain1)
             (domain-reqs domain2)))

    (let ((domain-name (or (domain-name domain1) (domain-name domain2)))
          (requirements (or (domain-reqs domain1)
                            (domain-reqs domain2)))
          merged-domain)

      ;; Create and return the merged domain description:
      (setf merged-domain
            (make-domain domain-name
                         :requirements requirements
                         :types (merge-domain-types domain1 domain2)
                         :predicates (merge-domain-predicates domain1 domain2)
                         :actions (merge-domain-actions pddl-expr1 pddl-expr2)
                         :constants (domain-constants domain1)))

      (add-to-domain-constants merged-domain (domain-constants domain2))
      (when (listp (first (domain-reqs merged-domain)))
        (error "The requirements list must be at the same level with the
               :REQUIREMENTS keyword in PDDL -- e.g., (:requirements :adl).
               The domain ~a has ~a. Check also the other PDDL objects for
               the same issue..."
               (domain-name merged-domain)
               `(:requirements ,@(domain-reqs merged-domain))))
      merged-domain)))

(defun problem-merger (pddl-expr1 pddl-expr2)
  (let ((problem1 pddl-expr1)
        (problem2 pddl-expr2))
    ;; The two expressions should describe parts of the same problem.
    (unless (or (eq (problem-name problem1) (problem-name problem2))
                (null (problem-name problem1))
                (null (problem-name problem2)))
      (error "Problems to be merged must be consistent in name.  ~A and ~A disagree."
             (problem-name problem1)
             (problem-name problem2)))

    ;; The two expressions should describe parts of the same domain.
    (unless (or (eq (problem-domain problem1) (problem-domain problem2))
                (null (problem-domain problem1))
                (null (problem-domain problem2)))
      (error "Domains to be merged must be consistent in name.  ~A and ~A disagree."
             (problem-domain problem1)
             (problem-domain problem2)))

    ;; Create and return the merged problem description:
    (make-problem (or (problem-name problem1) (problem-name problem2))
                  :domain (or (problem-domain problem1) (problem-domain problem2))
                  :objects (merge-problem-objects problem1 problem2)
                  :init (merge-problem-states problem1 problem2)
                  :goal (merge-problem-goals problem1 problem2))))
