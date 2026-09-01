(in-package :pddl-utils-tests)

(in-suite pddl-utils-tests)

;;; --- topological-sort-types ---

(test topological-sort-empty
  (is (null (topological-sort-types nil))))

(test topological-sort-flat
  "All types are direct children of OBJECT — order is alphabetical."
  (let ((input (pddlify-tree '(segment direction airplanetype - object))))
    (is (equal (topological-sort-types input)
               (pddlify-tree '(airplanetype direction segment - object))))))

(test topological-sort-chain
  "Linear chain: object > vehicle > airplanetype > airplane.
Supertypes must precede subtypes."
  (let ((input (pddlify-tree
                '(airplane - airplanetype
                  airplanetype - vehicle
                  vehicle - object))))
    (is (equal (topological-sort-types input)
               (pddlify-tree '(vehicle - object
                                airplanetype - vehicle
                                airplane - airplanetype))))))

(test topological-sort-mixed-depth
  "Siblings at different depths; out-of-order input."
  (let ((input (pddlify-tree
                '(airplane - airplanetype
                  airplanetype - vehicle
                  vehicle - object
                  segment direction - object))))
    (is (equal (topological-sort-types input)
               (pddlify-tree '(direction segment vehicle - object
                                airplanetype - vehicle
                                airplane - airplanetype))))))

(test topological-sort-implicit-supertype
  "A supertype that appears only on the right of - gets declared as child of OBJECT."
  (let ((input (pddlify-tree '(airplane - airplanetype))))
    (is (equal (topological-sort-types input)
               (pddlify-tree '(airplanetype - object
                                airplane - airplanetype))))))

(test topological-sort-untyped-defaults-to-object
  "Types without an explicit - supertype default to OBJECT."
  (let ((input (pddlify-tree '(foo bar))))
    (is (equal (topological-sort-types input)
               (pddlify-tree '(bar foo - object))))))

(test topological-sort-idempotent
  "Sorting an already-sorted list returns an equal list."
  (let ((sorted (pddlify-tree '(direction segment vehicle - object
                                  airplanetype - vehicle
                                  airplane - airplanetype))))
    (is (equal (topological-sort-types sorted) sorted))))

(test topological-sort-canonical-input
  "Accepts a fully canonical (one name per group) typed list."
  (let ((input (pddlify-tree '(a - object b - object c - a d - c))))
    (is (equal (topological-sort-types input)
               (pddlify-tree '(a b - object c - a d - c))))))

;;; --- (setf domain-types) ---

(test setf-domain-types-basic
  "Setting types on a domain topologically sorts them."
  (let ((domain (make-domain '#:test-domain
                             :requirements '(:typing)
                             :types '(foo - object)
                             :predicates '((p ?x - foo)))))
    (setf (domain-types domain)
          (pddlify-tree '(airplane - airplanetype
                           airplanetype - vehicle
                           vehicle - object
                           segment - object)))
    (is (equal (domain-types domain)
               (pddlify-tree '(segment vehicle - object
                                airplanetype - vehicle
                                airplane - airplanetype))))))

(test setf-domain-types-preserves-rest
  "Setting types does not disturb predicates or actions."
  (let* ((preds (pddlify-tree '((p ?x - foo) (q ?y - bar))))
         (domain (make-domain '#:test-domain
                              :requirements '(:typing)
                              :types '(foo bar - object)
                              :predicates preds)))
    (setf (domain-types domain)
          (pddlify-tree '(baz - object quux - baz)))
    (is (equal (domain-predicates domain) preds))
    (is (eq (domain-name domain) (pddl-symbol '#:test-domain)))))

(test setf-domain-types-airport
  "Round-trip: set the airport domain's types to the same values."
  (let* ((domain (read-planning-input
                  (merge-pathnames "airport-nontemporal-adl-domain.pddl"
                                   *tests-dir*)))
         (orig-types (copy-tree (domain-types domain))))
    (setf (domain-types domain) orig-types)
    (let ((new-types (domain-types domain)))
      (is (set-equal
         (typelist-to-alist (canonicalize-types orig-types))
         (typelist-to-alist (canonicalize-types new-types))
         :test #'equal))))))
