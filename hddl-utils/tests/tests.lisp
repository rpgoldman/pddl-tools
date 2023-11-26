(in-package :hddl-utils-tests)

;; defined in hddl-data.lisp
(declaim (special *complex-task-def* *complex-task-def2* *method*))

(defparameter *tests-dir*
  (namestring
   (translate-logical-pathname
    (asdf:system-relative-pathname :hddl-utils
                                   "hddl-utils/tests/"))))

(def-suite* hddl-tests)

(test complex-task-def
  (is-true (complex-task-sexp-p *complex-task-def*))
  (is-true (complex-task-sexp-p *complex-task-def2*)))

(test parse-method-lines
  (is (equalp (list "load truck-0 city-loc-1 package-0"
                    "m-load"
                    "1")
              (multiple-value-list (partition-method-line "load truck-0 city-loc-1 package-0 -> m-load 1"))))
  (is (equalp (list "deliver package-1 city-loc-2"
                    "m-deliver"
                    "16 13 17 11")
              (multiple-value-list (partition-method-line "deliver package-1 city-loc-2 -> m-deliver 16 13 17 11"))))
  (is (equalp
       '(hddl::load hddl::truck-0 hddl::city-loc-1 hddl::package-0)
       (hddl-io::space-separated-string->hddl-list "load truck-0 city-loc-1 package-0")))
  )

(test parse-domain
  (let ((parsed (read-hddl-file (asdf:system-relative-pathname "hddl-utils" "hddl-utils/tests/transport-domain-partial-order-ipc2020.hddl"))))
    (is-true parsed))
  ;; can we parse a total order domain
  (let ((parsed-domain (read-hddl-file (merge-pathnames "ipc2020-total-order-transport-domain.hddl" *tests-dir*))))
    (is-true parsed-domain)))

(test manipulate-domain-methods
  (let ((full-domain (read-hddl-file (asdf:system-relative-pathname "hddl-utils" "hddl-utils/tests/transport-domain-partial-order-ipc2020.hddl")))
        (empty-domain (read-hddl-file (asdf:system-relative-pathname "hddl-utils" "hddl-utils/tests/transport-domain-ipc2020-no-methods-or-actions.hddl"))))
    (let ((emptied-domain
            (remove-domain-actions (remove-domain-methods full-domain))))
      (is (equalp empty-domain emptied-domain)))
    (let ((method-spliced-domain (remove-domain-methods full-domain)))
      (is-true (typep method-spliced-domain 'domain))
      (is-false (domain-methods method-spliced-domain))
      (is (equalp (domain-actions method-spliced-domain) (domain-actions full-domain)))
      (is-true (typep full-domain 'domain))
      ;; there are no domain methods, but there are domain actions.
      (setf (domain-methods method-spliced-domain) (copy-tree (domain-methods full-domain)))
      (is (= (length full-domain) (length method-spliced-domain)))
      (is (equalp full-domain method-spliced-domain)))
    (let ((no-actions-domain (remove-domain-actions (remove-domain-methods full-domain))))
      (is-true (typep no-actions-domain 'domain))
      (is-false (domain-methods no-actions-domain))
      (is-false (domain-actions no-actions-domain))
      (is-true (typep no-actions-domain 'domain))
      (is-true (domain-methods full-domain))
      (is-true (domain-actions full-domain))
      (setf (domain-methods no-actions-domain) (copy-tree (domain-methods full-domain)))
      (is (equalp (remove-domain-actions full-domain) no-actions-domain)))
    (let ((empty-domain empty-domain))
      (is-true (typep empty-domain 'domain))
      (is-false (domain-methods empty-domain))
      (is-false (domain-actions empty-domain))
      (is-true (typep empty-domain 'domain))
      (is-true (domain-methods full-domain))
      (is-true (domain-actions full-domain))
      (setf (domain-methods empty-domain) (copy-tree (domain-methods full-domain)))
      (is (equalp (remove-domain-actions full-domain) empty-domain)))
    ))

(test manipulate-domain-actions
  (let ((full-domain (read-hddl-file (asdf:system-relative-pathname "hddl-utils" "hddl-utils/tests/transport-domain-partial-order-ipc2020.hddl")))
        (empty-domain (read-hddl-file (asdf:system-relative-pathname "hddl-utils" "hddl-utils/tests/transport-domain-ipc2020-no-methods-or-actions.hddl"))))
    (let ((some-actions-domain (reverse (cdr (reverse (copy-tree full-domain))))))
      (is-true (typep some-actions-domain 'domain))
      (is (= (length (domain-actions full-domain)) (1+ (length (domain-actions some-actions-domain)))))
      (setf (domain-actions some-actions-domain)
            (copy-tree (domain-actions full-domain)))
      (is (equalp full-domain some-actions-domain)))
    (let ((no-actions-domain (remove-domain-actions full-domain)))
      (is-true (typep no-actions-domain 'domain))
      (is-true (domain-methods no-actions-domain))
      (is-false (domain-actions no-actions-domain))
      (is-true (domain-methods full-domain))
      (is-true (domain-actions full-domain))
      (setf (domain-actions no-actions-domain) (copy-tree (domain-actions full-domain)))
      (is (equalp full-domain no-actions-domain)))
    (let ((empty-domain empty-domain)
          (no-methods-domain (remove-domain-methods full-domain)))
      (is-true (typep empty-domain 'domain))
      (is-false (domain-methods empty-domain))
      (is-false (domain-actions empty-domain))
      (is-true (typep no-methods-domain 'domain))
      (is-false (domain-methods no-methods-domain))
      (is-true (domain-actions no-methods-domain))
      (setf (domain-actions empty-domain) (copy-tree (domain-actions full-domain)))
      (is (equalp no-methods-domain empty-domain)))
    ))

(test manipulate-domain-tasks
  (let ((full-domain (read-hddl-file (asdf:system-relative-pathname "hddl-utils" "hddl-utils/tests/transport-domain-partial-order-ipc2020.hddl")))
        (empty-domain (read-hddl-file (asdf:system-relative-pathname "hddl-utils" "hddl-utils/tests/transport-domain-ipc2020-no-methods-tasks-or-actions.hddl"))))
    ;; splice tasks into a domain that doesn't have any
    (let ((task-spliced-domain (remove-domain-tasks full-domain)))
      (is-true (typep task-spliced-domain 'domain))
      (is-false (domain-tasks task-spliced-domain))
      (is-true (domain-tasks full-domain))
      (is (equalp (domain-actions task-spliced-domain)
                  (domain-actions full-domain)))
      (is (equalp (domain-methods task-spliced-domain)
                  (domain-methods full-domain)))
      (is-true (typep full-domain 'domain))
      (setf (domain-tasks task-spliced-domain)
            (copy-tree (domain-tasks full-domain)))
      (is (= (length full-domain) (length task-spliced-domain)))
      (is (equalp full-domain task-spliced-domain)))
    ;; test to see that this works properly if there are no methods
    (let ((empty-domain empty-domain))
      (is-true (typep empty-domain 'domain))
      (is-false (domain-methods empty-domain))
      (is-false (domain-actions empty-domain))
      (is-false (domain-tasks empty-domain))
      (is-true (typep empty-domain 'domain))
      (setf (domain-tasks empty-domain)
            (copy-tree (domain-tasks full-domain)))
      (is (equalp (remove-domain-actions (remove-domain-methods full-domain))
                  empty-domain)))
    (let ((no-methods-domain (remove-domain-tasks (remove-domain-methods full-domain))))
      (is-true (typep no-methods-domain 'domain))
      (is-false (domain-methods no-methods-domain))
      (is-true (domain-actions no-methods-domain))
      (is-true (typep no-methods-domain 'domain))
      (setf (domain-tasks no-methods-domain) (copy-tree (domain-tasks full-domain)))
      (is (equalp (remove-domain-methods full-domain)
                  no-methods-domain)))
    (let ((header-only-domain (hddl-utils:make-domain (domain-name full-domain)))
          (expected-domain (hddl-utils:make-domain (domain-name full-domain)
                                                   :tasks (copy-list (domain-tasks full-domain)))))
      (setf (domain-tasks header-only-domain) (copy-list (domain-tasks full-domain)))
      (is (equalp expected-domain header-only-domain)))))

(in-package :hddl)

(cl:defparameter hddl-utils-tests::*expected-state*
  '(
    (capacity_predecessor capacity_0 capacity_1)
    (road city_loc_0 city_loc_1)
    (road city_loc_1 city_loc_0)
    (road city_loc_1 city_loc_2)
    (road city_loc_2 city_loc_1)
    (at package_0 city_loc_1)
    (at package_1 city_loc_1)
    (at truck_0 city_loc_2)
    (capacity truck_0 capacity_1)
    ))

(cl::defparameter hddl-utils-tests::*expected-objects*
  '(package_0
    package_1
    capacity_0
    capacity_1
    city_loc_0
    city_loc_1
    city_loc_2
    truck_0))

(cl::defparameter hddl-utils-tests::*expected-goal*
  '(and (at package_0 city_loc_0)
    (at package_1 city_loc_2)))

(cl::defparameter hddl-utils-tests::*expected-po-goal*
  '(and
   (walked couple0 place2)
   (walked couple1 place2)
   (walked couple2 place2)
   ))

(cl:in-package :hddl-utils-tests)

(test parse-problem
  (let ((problem (read-hddl-file (merge-pathnames "ipc2020-total-order-transport-p01.hddl" *tests-dir*))))
    (is-true problem)
    (is (null (problem-goal problem)))
    (is (alexandria:set-equal
         *expected-objects*
         (remove-types-from-list (problem-objects problem))))
    (is-true (typep problem 'hddl:problem))
    (is (eq 'hddl::domain_htn (hddl-utils:problem-domain problem)))
    (is (set-equal *expected-state*
                   (hddl-utils:problem-state problem)
                   :test #'equalp))
    (is (null (hddl-utils:problem-goal problem)))))

(test problem-goals-ordered
  (let ((problem (read-hddl-file (merge-pathnames "ipc2020-total-order-transport-p01.hddl" *tests-dir*)))
        (goal-expr *expected-goal*))
    (setf (hddl-utils:problem-goal problem) goal-expr)
    (is (equalp goal-expr (hddl-utils:problem-goal problem)))
    )
  )

(test problem-goals-partial-order
  (let ((problem (read-hddl-file (merge-pathnames "ipc2020-hiking-ordered-p01.hddl" *tests-dir*)))
        (goal-expr *expected-goal*))
    (is (equal *expected-po-goal*
               (hddl-utils:problem-goal problem)))
    (setf (hddl-utils:problem-goal problem) goal-expr)
    (is (equalp goal-expr (hddl-utils:problem-goal problem)))
    )
  )



(test check-reading-plan
  (let ((plan (hddl-utils:read-hddl-plan-file
               (asdf:system-relative-pathname "hddl-utils" "hddl-utils/tests/example-plan.hddl"))))
    (is (equalp *parsed-plan* plan))))

(test check-reading-plan-sexp
  (let ((plan (hddl-utils:read-hddl-plan-file
               (asdf:system-relative-pathname "hddl-utils" "hddl-utils/tests/example-plan-sexp.hddl"))))
    (is (equalp *parsed-plan* plan))))

(test check-comma-separated-task
  (let ((string "drive truck-0, city-loc-2, city-loc-1"))
    (is (equalp *expected-task*
                (hddl-pprinter::comma-separated-task->sexp string)))))

(test check-reading-plan-comma-separated
  (let ((plan (hddl-utils:read-hddl-plan-file
               (asdf:system-relative-pathname "hddl-utils" "hddl-utils/tests/example-plan-comma-separated.hddl"))))
    (is (equalp *parsed-plan* plan))))

(test method-subtasks
  ;; set subtasks to method without them
  (let ((method (copy-tree *method-no-subtasks*)))
    (setf (method-subtasks method)
          (copy-tree *method-subtasks*))
    (is (equalp *full-method* method)))
  ;; reset subtasks
  (let ((method (copy-tree *method-different-subtasks*)))
    (setf (method-subtasks method)
          (copy-tree *method-subtasks*))
    (is (equalp *full-method* method)))
  ;; :tasks instead of :ordered-subtasks
  (let ((method (copy-tree *method-different-subtasks*))
        (template (copy-tree *full-method*)))
    (setf method (subst :tasks :ordered-subtasks method)
          template (subst :tasks :ordered-subtasks template))
    (setf (method-subtasks method)
          (copy-tree *method-subtasks*))
    (is (equalp template method)))
  )
