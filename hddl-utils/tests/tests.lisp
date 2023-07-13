(in-package :common-lisp-user)

(defpackage :hddl-utils-tests
  (:use common-lisp hddl-utils fiveam)
  (:import-from :hddl-io #:partition-method-line
                #:read-hddl-file
                #:complex-task-sexp-p))

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
    (is-true parsed)))

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
        (empty-domain (read-hddl-file (asdf:system-relative-pathname "hddl-utils" "hddl-utils/tests/transport-domain-ipc2020-no-methods-or-actions.hddl"))))
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
