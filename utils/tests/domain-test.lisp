(in-package :pddl-utils-tests)

(defparameter *tests-dir*
  (namestring
   (translate-logical-pathname
    (asdf:system-relative-pathname :pddl-utils
                                   "utils/tests/"))))

(defun read-planning-input (filename)
  (read-pddl-file filename))

(defun problem-free-p (type-el typed-list)
  (let ((pos (position type-el typed-list
                       :test #'equal)))
    (when pos
      (if (and (not (= pos 0))
               (eql (nth (1- pos) typed-list) '-))
        ;; then the TYPE-EL is a parent type itself.
        ;; Skip this occurrence of the symbol in the TYPED-LIST. 
        (problem-free-p type-el (subseq typed-list (1+ pos)))
        ;; Otherwise, TYPE-EL is a subtype so look for if we have its
        ;; parent. 
        (let ((next-parent-type
               (position '- (subseq typed-list (1+ pos)))))
          (if (null next-parent-type)
            (if (eql type-el 'object)
              ;; then we're good.
              (return-from problem-free-p t)
              ;; otherwise,there is a top-level type that is not OBJECT,
              ;; which should not be correct given our
              ;; CANONICALIZE-TYPES reformulation.
              (return-from problem-free-p nil))
            ;; Else there is a parent type. That's ok, but check if this
            ;; TYPE-EL belongs to another parent now...
            (problem-free-p type-el 
                            (subseq typed-list (1+
                                                next-parent-type)))))))
    t))

(def-fixture domains ()
  (let* (
         #|(domain-extensions
         (read-planning-input
         "obtw:S3;disturbance-analyzer;counter-domains;airport;airportlog-debugging-domain.pddl")) |#
         (orig-domain
           (read-planning-input
            (merge-pathnames "airport-nontemporal-adl-domain.pddl"
                             *tests-dir*)))
         (openstacks-domain
           (read-planning-input
            (merge-pathnames "openstacks-strips.pddl"
                             *tests-dir*)))
         #|
         (merged-domain
         (make-debugging-domain (remove-domain-actions orig-domain) domain-extensions))
         |#
         )
    (&body)))


(def-fixture simple-problem ()
  (let ((problem (read-pddl-file
                  (merge-pathnames "airport-nontemporal-adl-p01_airport1_p1.pddl"
                                   *tests-dir*))))
    (&body)))

;; This could also go into the SIMPLE-PROBLEM fixture/test above, but I'm doing it
;; seperately for now since I'm just getting familiar with FiveAM facilities.
;; [2011/11/22:uk].
(def-fixture well-defined-pddl-objects ()
  (let ((domain
          (read-planning-input
           (merge-pathnames "airport-nontemporal-adl-domain.pddl"
                            *tests-dir*))))
    (&body)))

(def-fixture well-defined-predicate-settings ()
  (let ((domain
          (read-planning-input
           (merge-pathnames "airport-nontemporal-adl-domain.pddl"
                            *tests-dir*))))
    (setf (domain-predicates domain) 
          (append (domain-predicates domain)
                  (list '(DONE))))
    (&body)))


(test types-correct-p
      (with-fixture 
          well-defined-pddl-objects ()
          ;; Check for simple subtyping -- every
          ;; type should have a parent type, except the type
          ;; OBJECT.
          (is (every #'(lambda (element)
                         (problem-free-p element
                                         (copy-tree (domain-types domain))))
                     (domain-types domain)))))
                           
(test predicate-definitions-correct-p
      (with-fixture
          well-defined-pddl-objects ()
          ;; Check for duplicates...
          (is (every #'(lambda (element)
                         (let ((element-trail 
                                (member element (domain-predicates
                                                 domain)
                                        :test #'equal)))
                           (member element element-trail
                                   :test #'equal)))
                     (domain-predicates domain)))))

(test predicate-DONE-member-p
      (with-fixture
          well-defined-predicate-settings ()
          (is (member '(pddl::DONE) (domain-predicates domain)
                      :test #'equal))))

(test domain-well-defined-p
  (with-fixture 
      well-defined-pddl-objects ()
      (is (every #'keywordp (domain-reqs domain)))
      (is (every #'(lambda (el) 
                     (member el *pddl-keywords* :test #'eql))
                 (domain-reqs domain)))))

(test problem-acc
  (with-fixture simple-problem ()
    (is (eq (problem-name problem) (pddlify '#:problem_x)))
    (is (eq (problem-domain problem) (pddlify '#:airport)))
    (is (equal (problem-objects problem)
               (pddlify-tree
                '(      airplane_CFBEG - airplane
                  medium - airplanetype
                  north
                  south - direction
                  seg_pp_0_60
                  seg_ppdoor_0_40
                  seg_tww1_0_200
                  seg_twe1_0_200
                  seg_tww2_0_50
                  seg_tww3_0_50
                  seg_tww4_0_50
                  seg_rww_0_50
                  seg_rwtw1_0_10
                  seg_rw_0_400
                  seg_rwe_0_50
                  seg_twe4_0_50
                  seg_rwte1_0_10
                  seg_twe3_0_50
                  seg_twe2_0_50
                  seg_rwte2_0_10
                  seg_rwtw2_0_10 - segment
                  ))))
    (is (equal (problem-goal problem)
               (pddlify-tree '(AND (is-parked airplane_CFBEG seg_pp_0_60)))))
    (is (let ((new-problem (copy-tree problem))
              (new-goal (pddlify-tree '(is-moving airplane_CFBEG))))
          (setf (problem-goal new-problem) new-goal)
          (and
           (equal (problem-goal problem)
                  (pddlify-tree '(AND (is-parked airplane_CFBEG seg_pp_0_60))))
           (equal (problem-goal new-problem)
                  new-goal))))
    (is (equal (problem-state problem) *p01-state*))
    (is (equal
         (let ((newprob (copy-tree problem)))
           (setf (problem-state newprob)
                 (subst 'pddl::ga1 'pddl::airplane_cfbeg
                        (problem-state newprob)))
           (problem-state newprob))
         *substituted-p01-state*))))

(test domain-acc
  (with-fixture domains ()
    (is
     (equal
      (remove-domain-actions orig-domain)
      *actionless-domain*))
    (is (eq (domain-name orig-domain)
            (pddlify '#:airport)))
    (is
     (equal (domain-reqs orig-domain) '(:adl)))
    (is
     (equal (domain-types orig-domain)
            (pddlify-tree '(segment direction airplanetype - object
                            vehicle - object ; new type
                            airplane - vehicle))))
    (is
     (equal (domain-predicates orig-domain)
            (pddlify-tree
             '((can-move ?s1 ?s2 - segment ?d - direction)
               (can-pushback ?s1 ?s2 - segment ?d - direction)
               (move-dir ?s1 ?s2 - segment ?d - direction)
               (move-back-dir ?s1 ?s2 - segment ?d - direction)
               (is-blocked ?s1  - segment ?t - airplanetype ?s2 - segment ?d - direction)
               (has-type ?a - airplane ?t - airplanetype)
               (at-segment ?v - vehicle ?s - segment)
               (facing ?a - airplane ?d - direction)
               (occupied ?s - segment)
               (blocked ?s - segment ?a - vehicle)
               (is-start-runway ?s - segment ?d - direction)
               (airborne ?a - airplane ?s - segment)
               (is-moving ?a - airplane)
               (is-pushing ?a - airplane)
               (is-parked ?a - airplane ?s - segment)
               ))))
    (is
     (equal (domain-actions orig-domain)
            *airport-action-list*))
    (is (null (domain-functions orig-domain)))
    (is (equal (domain-constants openstacks-domain)
               (pddlify-tree `(p1 p2 p3 p4 p5 - product
                                  o1 o2 o3 o4 o5 - order))))))

(test action-p
  (is
   (every #'(lambda (x) (typep x 'pddl-utils:action))
          *airport-action-list*)))



