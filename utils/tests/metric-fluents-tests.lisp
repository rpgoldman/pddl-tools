(in-package :pddl-utils-tests)

(def-fixture rover-domain ()
  (let ((domain (read-pddl-file (asdf:system-relative-pathname "pddl-utils" "utils/tests/numerical-rover.pddl"))))
    (&body)))

(def-fixture navigate-action ()
  (let ((act (domain-action domain 'pddl::navigate)))
    (&body)))

(def-fixture rover-problem ()
  (let ((problem (read-pddl-file (asdf:system-relative-pathname "pddl-utils" "utils/tests/numerical-rover-pfile01.pddl"))))
    (&body)))

(test check-numerical-domain-items
  (with-fixture rover-domain ()
    (is (alexandria:set-equal
         '(sift-pddl::rover sift-pddl::waypoint sift-pddl::store
           sift-pddl::camera sift-pddl::mode sift-pddl::lander
           sift-pddl::objective sift-pddl:object)
         (pddl-utils::all-types (domain-types domain))))
    (is (alexandria:set-equal
         'sift-pddl::(energy recharges)
         (mapcar #'first (domain-functions domain))))
    ;; check some metric preconditions and effects...
    (with-fixture navigate-action ()
      (is (eq 'and (first (action-precondition act))))
      (is (member 'pddl::(>= (energy ?x) 8) (rest (action-precondition act))
                  :test 'equalp))
      (is (eq 'and (first (action-effect act))))
      (is (member 'pddl::(decrease (energy ?x) 8) (rest (action-effect act))
                  :test 'equalp)))))


(test check-numerical-problem-items
  (with-fixture rover-problem ()
    (is (member 'pddl::(= (recharges) 0) (problem-state problem) :test 'equalp))
    (is (member 'pddl::(= (energy rover0) 0) (problem-state problem) :test 'equalp))))
