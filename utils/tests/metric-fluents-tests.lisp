(in-package :pddl-utils-tests)

(in-suite pddl-utils-tests)

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
         (pddlify-tree
          '(rover waypoint store
            camera mode lander
            objective object))
         (pddl-utils::all-types (domain-types domain))))
    (is (alexandria:set-equal
         (pddlify-tree '(energy recharges))
         (mapcar #'first (domain-functions domain))))
    ;; check some metric preconditions and effects...
    (with-fixture navigate-action ()
      (is (eq 'and (first (action-precondition act))))
      (is-true (member (pddlify-tree '(>= (energy ?x) 8)) (rest (action-precondition act))
                  :test 'equalp))
      (is (eq 'and (first (action-effect act))))
      (is-true (member (pddlify-tree '(decrease (energy ?x) 8)) (rest (action-effect act))
                  :test 'equalp)))))


(test check-numerical-problem-items
  (with-fixture rover-problem ()
    (let ((sought-fact (pddlify-tree '(= (recharges) 0))))
      (is-true (member sought-fact (problem-state problem) :test 'equalp)
               "Unable to find ~a in state:~%~{~S~%~}"
               sought-fact
               (problem-state problem)))
    (let ((sought-fact (pddlify-tree '(= (energy rover0) 50))))
      (is-true (member sought-fact (problem-state problem) :test 'equalp)
               "Unable to find ~a in state:~%~{~S~%~}"
               sought-fact
               (problem-state problem)))))
